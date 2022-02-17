{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Frontend.WalletConnect where

import Control.Monad (join, void, forM_)
import Control.Monad.IO.Class
import Control.Monad.Fix
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
import Language.Javascript.JSaddle (valToJSON, liftJSM, JSM)

import Kadena.SigningApi

import WalletConnect.Wallet
import Frontend.AppCfg
import Frontend.Wallet (accountKeys, unAccountData)
import Common.Wallet

setupWalletConnect :: (_)
  => m (WalletConnect t
     , (FRPHandler SigningRequest SigningResponse t)
     , (FRPHandler QuickSignRequest QuickSignResponse t)
     )
setupWalletConnect = do
  walletConnect <- initWalletConnect Nothing "Kadena"
  let
    ev = ffor (_walletConnect_requests walletConnect) $ \(t, req, reply) ->
      let
        resp :: (A.ToJSON b) => Either a b -> JSM ()
        resp = either (const $ reply $ Left ()) (reply . Right . A.toJSON)
      in case A.fromJSON (_request_params req) of
        A.Success r -> Right (Left (r, resp))
        A.Error e1 -> case A.fromJSON (_request_params req) of
          A.Success r -> Right (Right (r, resp))
          A.Error e2 -> Left (e1, e2)
    sucEv = fmapMaybe (either (const Nothing) Just) ev
    errEv = fmapMaybe (either Just (const Nothing)) ev
    signingHandler = fmapMaybe (either Just (const Nothing)) sucEv
    quickSignHandler = fmapMaybe (either (const Nothing) Just) sucEv

  performEvent $ ffor (_walletConnect_requests walletConnect) $ \(t, req, reply) -> liftIO $ do
    putStrLn $ "Recieved request"
    putStrLn $ show $ _request_params req

  performEvent $ ffor quickSignHandler $ \(r, _) -> liftIO $ do
    putStrLn $ "Recieved quicksign request"
    putStrLn $ show $ _quickSignRequest_commands r
  performEvent $ ffor errEv $ \(e1, e2) -> liftIO $ do
    putStrLn $ "Error in request decoding: e1: " <> e1
    putStrLn $ "Error in request decoding: e2: " <> e2

  pure (walletConnect, signingHandler, quickSignHandler)

handleWalletConnectPairings accounts walletConnect = do
  let ev = attach (current accounts) (_walletConnect_proposals walletConnect)

  performEvent_ $ ffor ev $ \(d, Proposal t m p approve) -> do
    let

      -- accSet = Set.unions $ map accountKeys (foldMap M.elems $ map _accountInfo_chains $ foldMap M.elems $ M.elems $ unAccountData d)
      accSet = Set.fromList $ map (\n -> fromMaybe n (T.stripPrefix "k:" n)) $
        map unAccountName $ foldMap M.keys $ M.elems $ unAccountData d
      -- accSet = Set.fromList $ map toName (foldMap M.assocs $ map _accountInfo_chains $ foldMap M.elems $ M.elems $ unAccountData d)
      -- toName (chainId, account) = T.pack (show chainId) <> ":" <> keyToText account
    liftIO $ putStrLn $ "Auto Approving : " <> T.unpack t
    -- The react-app require an account for the chain it currently supports, hence eip115:42
    let accounts = map ("kadena:0:" <>) $ Set.toList accSet
    liftIO $ putStrLn $ show $ accounts

    liftJSM $ approve $ Right accounts

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
