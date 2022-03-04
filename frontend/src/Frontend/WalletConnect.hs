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
import Frontend.Wallet (accountKeys, unAccountData)
import Frontend.UI.Dialogs.WalletConnect
import Reflex.Notifications
import Common.Wallet

setupWalletConnect :: (_)
  => m (WalletConnect t
     , m (FRPHandler (SigningRequest, Maybe Metadata) SigningResponse t)
     , m (FRPHandler (QuickSignRequest, Maybe Metadata) QuickSignResponse t)
     )
setupWalletConnect = do
  walletConnect <- initWalletConnect Nothing "Kadena"
  let
    ev0 = attach (current $ _walletConnect_sessions walletConnect) (_walletConnect_requests walletConnect)
    ev = ffor ev0 $ \(sessions, (t, req, reply)) ->
      let
        mMeta = snd . _session_peer <$> M.lookup t sessions
        resp :: (A.ToJSON b) => Either a b -> JSM ()
        resp = either (const $ reply $ Left ()) (reply . Right . A.toJSON)
      in case A.fromJSON (_request_params req) of
        A.Success r -> Right (Left ((r, mMeta), resp))
        A.Error e1 -> case A.fromJSON (_request_params req) of
          A.Success r -> Right (Right ((r, mMeta), resp))
          A.Error e2 -> Left (e1, e2)
    sucEv = fmapMaybe (either (const Nothing) Just) ev
    errEv = fmapMaybe (either Just (const Nothing)) ev
    signingEv = fmapMaybe (either Just (const Nothing)) sucEv
    quickSignEv = fmapMaybe (either (const Nothing) Just) sucEv

  performEvent $ ffor (_walletConnect_requests walletConnect) $ \(t, req, reply) -> liftIO $ do
    putStrLn $ "Recieved request"
    putStrLn $ show $ _request_params req

  performEvent $ ffor quickSignEv $ \(r, _) -> liftIO $ do
    putStrLn $ "Recieved quicksign request"
    putStrLn $ show $ _quickSignRequest_commands $ fst r
  performEvent $ ffor errEv $ \(e1, e2) -> liftIO $ do
    putStrLn $ "Error in request decoding: e1: " <> e1
    putStrLn $ "Error in request decoding: e2: " <> e2

  signingHandler <- mkBufferedFRPHandler signingEv
  quickSignHandler <- mkBufferedFRPHandler quickSignEv

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

  pure (walletConnect, signingHandler, quickSignHandler)

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
