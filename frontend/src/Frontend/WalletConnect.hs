{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
module Frontend.WalletConnect
  ( walletConnectTestWidget
  , walletConnectTopWidget
  , doInit
  , WalletConnect(..)
  , Request(..)
  , Proposal(..)
  )
  where

import Control.Monad (join, void, forM_)
import Control.Monad.IO.Class
import Control.Monad.Fix
import qualified Data.Aeson as Aeson
import Data.Maybe (listToMaybe)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom hiding (Request)
import Pact.Server.ApiClient (runTransactionLoggerT, noLogger)
import Obelisk.Frontend
import Obelisk.Route.Frontend
import Obelisk.Generated.Static
import System.IO
import Language.Javascript.JSaddle (valToJSON, liftJSM)

import Frontend.WalletConnect.Internal as WalletConnect
import Kadena.SigningApi (SigningRequest, SigningResponse)

walletConnectTestWidget = do
  uriInp <- inputElement $ def
  pairEv <- button "Pair"

  WalletConnect.WalletConnect sessions proposalEv reqEv doPair <- WalletConnect.doInit
    -- (Just "ws://192.168.11.15")
    Nothing
    "some-project"

  performEvent $ ffor (tag (current $ value uriInp) pairEv) $ (liftIO . doPair)

  -- display $ M.keys <$> sessions
  dyn $ ffor sessions $ \ss -> do
    forM_ ss $ \session -> do
      ev <- button "disconnect"
      performEvent $ ffor ev $ \_ -> liftJSM $ WalletConnect._session_disconnect session

  widgetHoldWithRemoveAfterEvent $ ffor proposalEv $ \(WalletConnect.Proposal t m p approve) -> do
    ev1 <- (Right ["eip155:42:0x8fd00f170fdf3772c5ebdcd90bf257316c69ba45"] <$) <$> button "approve"
    ev2 <- (Left () <$) <$> button "reject"
    performEvent $ ffor (leftmost [ev1, ev2]) $ \v -> do
      liftJSM $ approve v

  widgetHoldWithRemoveAfterEvent $ ffor reqEv $ \(_, WalletConnect.Request _ m params, resp) -> do
    let r = Aeson.toJSON ()
    ev1 <- (Right r <$) <$> button "reply"
    ev2 <- (Left () <$) <$> button "reject"
    text m
    text $ T.pack $ show params
    performEvent $ ffor (leftmost [ev1, ev2]) $ \v -> do
      liftJSM $ resp v

  pure ()

widgetHoldWithRemoveAfterEvent
  :: (MonadFix m, MonadHold t m, DomBuilder t m) => Event t (m (Event t a)) -> m (Event t a)
widgetHoldWithRemoveAfterEvent wEv = do
  let f1 w = do
        rec let ev = (switch . current) evDyn
            evDyn <- widgetHold (w) (return never <$ ev)
        return ev
  evDyn <- widgetHold (return never) (f1 <$> wEv)
  return $ (switch . current) evDyn

walletConnectTopWidget (WalletConnect.WalletConnect sessions proposalEv _ doPair) mUri = do

  forM_ mUri $ \uri -> do
    rec
      pairEv <- (switch . current) <$> widgetHold (button "Proceed with pairing?")
        (return never <$ pairEv)
    performEvent $ ffor pairEv $ \_ -> liftIO $ doPair uri

  -- TODO: Is this proposal for the given URI?
  -- widgetHold (text "waiting") $ ffor proposalEv $ \(WalletConnect.Proposal t m p approve) -> do
  --   let accounts = ["eip155:42:0x8fd00f170fdf3772c5ebdcd90bf257316c69ba45"]
  --   ev1 <- (Right accounts <$) <$> button "approve"
  --   ev2 <- (Left () <$) <$> button "reject"
  --   done <- performEvent $ ffor (leftmost [ev1, ev2]) (liftJSM . approve)
  --   pure ()

  dyn $ ffor sessions $ \ss -> el "table" $ do
    forM_ ss $ \session -> el "tr" $ do
      el "td" $ text $ T.pack $ show $ _session_peer session
      ev <- el "td" $ button "disconnect"
      performEvent $ ffor ev $ \_ -> liftJSM $ WalletConnect._session_disconnect session
  pure ()
