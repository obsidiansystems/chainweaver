{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Frontend.WalletConnect where

import Control.Monad (join, void, forM_, guard)
import Control.Monad.IO.Class
import Control.Monad.Fix
import Control.Lens
import qualified Data.Aeson as A
import Data.Maybe (listToMaybe, fromMaybe)
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom hiding (Request)
import Pact.Server.ApiClient (runTransactionLoggerT, noLogger)
import Obelisk.Frontend
import Obelisk.Route.Frontend
import Obelisk.Generated.Static
import System.IO
import Language.Javascript.JSaddle (valToJSON, liftJSM, JSM, fun, jsg, js0)

import Kadena.SigningApi

import WalletConnect.Wallet
import Frontend.AppCfg
import Frontend.Foundation hiding (Request)
import Frontend.Wallet (accountKeys, unAccountData)
import Frontend.UI.Dialogs.WalletConnect
import Reflex.Notifications hiding (tag)
import Common.Wallet

walletConnectProjectId :: Text
walletConnectProjectId = "2155753170ee50854f546620185aeaed"

setupWalletConnect :: (_)
  => m (WalletConnect t
     , m ( (FRPHandler (SigningRequest, Maybe Metadata) SigningResponse t)
         , (FRPHandler (QuickSignRequest, Maybe Metadata) QuickSignResponse t)
         )
     , Event t (Maybe Metadata, String, Request)
     )
setupWalletConnect = do
  walletConnect <- initWalletConnect Nothing walletConnectProjectId
  let
    ev0 = attach (current $ _walletConnect_sessions walletConnect) (_walletConnect_requests walletConnect)
    ev = ffor ev0 $ \(sessions, (t, req, reply)) ->
      let
        mMeta = snd . _session_peer <$> M.lookup t sessions
        resp :: (A.ToJSON b) => Either a b -> JSM ()
        resp = either (const $ reply $ Left ()) (reply . Right . A.toJSON)
      in if (_request_method req == "kadena_sign")
         then case A.fromJSON (_request_params req) of
           A.Success r -> Right (Left ((r, mMeta), resp))
           A.Error e -> Left (mMeta, e, req)
         else case A.fromJSON (_request_params req) of
           A.Success r -> Right (Right ((r, mMeta), resp))
           A.Error e -> Left (mMeta, e, req)
    sucEv = fmapMaybe (either (const Nothing) Just) ev
    signingEv = fmapMaybe (either Just (const Nothing)) sucEv
    quickSignEv = fmapMaybe (either (const Nothing) Just) sucEv
    errEv = fmapMaybe (either Just (const Nothing)) ev

  -- The following code ensures that a queue of incoming signing requests is
  -- maintained, so that multiple incoming request events dont override the
  -- modal while the user is in the process of doing signing, and also to ensure
  -- that when the wallet is locked the signing requests are stored in memory.
  -- Once the wallet is unlocked the signing request events are emitted 'one at
  -- a time', ie it waits for the user to respond to the request, and after a
  -- delay fires the event of next request in the queue.
  signingHandler <- do
    let ev = leftmost [Left <$> signingEv, Right <$> quickSignEv]
    addEv <- numberOccurrences (Just <$> ev)
    (removeEv, removeAction) <- newTriggerEvent
    pendingMsgs <- foldDyn (\(k,v) -> M.alter (const v) k) mempty $
      leftmost [addEv, removeEv]
    pure $ mdo
      pb <- delay 0 =<< getPostBuild
      recheckEv <- delay 1 $
        leftmost [ () <$ removeEv
                 , fmapMaybe (bool Nothing (Just ())) isEmptyEv
                 ]
      let
        msgsEv = attach (current lastReq) $ tag (current pendingMsgs) $
          leftmost [recheckEv, pb]

        isEmptyEv = M.null . snd <$> msgsEv -- does retrigger after delay

        makeReqEv (mLast, m) = triggerRemove mLast =<< M.lookupMin m

        triggerRemove mLast (i, eitherV) =
          let
            doRemove :: (_) => _ -> _
            doRemove (req, resp) = (req,) $ \v ->
              (liftIO $ removeAction (i, Nothing)) >> resp v
          in if mLast < Just i
            then Just (i, bimap doRemove doRemove eitherV)
            else Nothing

        reqEv = fmapMaybe makeReqEv msgsEv
      lastReq <- holdDyn Nothing (Just . fst <$> reqEv)
      pure $ fanEither $ snd <$> reqEv

  askPermissionEv <- delay 0 =<< getPostBuild
  let
    sendNotificationEv = title <$ leftmost [() <$ signingEv, () <$ quickSignEv]
    title = "You have an incoming signing request."
    notification :: Notification Int
    notification = Notification
      { onclick = Just $ fun $ \_ _ _ -> do -- TODO: confirm if this works
          w <- jsg ("window" :: Text)
          void $ w ^. js0 ("focus" :: Text)
      , onclose = Nothing
      , onerror = Nothing
      , onshow = Nothing
      , options = defaultNotificationOptions
        { body = "Open your wallet to view and sign the transaction."
        , icon = static @"img/favicon/favicon-96x96.png"
        , badge = static @"img/favicon/favicon-16x16.png"
        }
      , contents = title
      }
  txtEv <- withUserPermission askPermissionEv
    (do
      sendNotification $ notification <$ sendNotificationEv
      pure "Notification permission was granted by the user"
      )
    (pure . (\e -> "Error in obtaining notification permission: " <> show e))
  -- TODO log txtEv

  pure (walletConnect, signingHandler, errEv)

handleWalletConnectPairings pubKeys network walletConnect = do
  let
    withNetwork = attach (current network) (_walletConnect_proposals walletConnect)
  let approvedMethods = ["kadena_sign", "kadena_quicksign"]

  -- Reject the session without user input if the permissions are not correct
  proposalEv <- performEvent $ ffor withNetwork $ \(n, p) -> do
    let
      wrongMethods = filter (not . (flip elem $ approvedMethods)) methods
      wrongChains = filter (/= walletConnectChainId n) chains
      Permissions chains methods = _proposal_permissions p
    if null wrongMethods && null wrongChains
      then pure $ Right (n, p)
      else do
        liftJSM $ (_proposal_approval p) (Left ())
        pure $ Left (wrongMethods, wrongChains, n, p)

  let
    (proposalErrorEv, checkedProposalEv) = fanEither proposalEv
    modalEv1 = uiWalletConnectSessionProposal <$> attach (current pubKeys) checkedProposalEv
    modalEv2 = uiWalletConnectSessionPermissionError <$> proposalErrorEv

  pure $ leftmost [modalEv1, modalEv2]

walletConnectTopWidget wc@(WalletConnect pairings sessions _ _ _) mUri = do

  forM_ mUri $ \uri -> do
    rec
      pairEv <- (switch . current) <$> widgetHold (button "Proceed with pairing?")
        (return never <$ pairEv)
    resultEv <- doNewPairing wc (uri <$ pairEv)
    widgetHold_ blank $ ffor pairEv $ \_ -> do
      widgetHold_ (text "Waiting for pairing to complete") $ ffor resultEv $ \case
        True -> text "Pairing Succeeded"
        False -> text "Pairing Failed"

    pure ()

  -- widgetHold (text "waiting") $ ffor proposalEv $ \(WalletConnect.Proposal t m p approve) -> do
  --   let accounts = ["eip155:42:0x8fd00f170fdf3772c5ebdcd90bf257316c69ba45"]
  --   ev1 <- (Right accounts <$) <$> button "approve"
  --   ev2 <- (Left () <$) <$> button "reject"
  --   done <- performEvent $ ffor (leftmost [ev1, ev2]) (liftJSM . approve)
  --   pure ()

  let showMetaData m = do
        el "h4" $ text $ _metadata_name m
        el "p" $ text $ _metadata_url m
        el "p" $ text $ _metadata_description m

  el "h2" $ text "Sessions"
  dyn $ ffor sessions $ \ss -> el "table" $ do
    forM_ ss $ \session -> el "tr" $ do
      let m = snd $ _session_peer session
      el "td" $ showMetaData m
      ev <- el "td" $ button "disconnect"
      performEvent $ ffor ev $ \_ -> liftJSM $ _session_disconnect session

  el "h2" $ text "Pairings"
  dyn $ ffor pairings $ \pp -> el "table" $ do
    forM_ pp $ \pairing -> el "tr" $ do
      let m = snd $ _pairing_peer pairing
      el "td" $ showMetaData m
      ev2 <- el "td" $ button "Delete"
      performEvent $ ffor ev2 $ \_ -> liftJSM $ (_pairing_delete pairing)

  pure ()
