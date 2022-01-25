{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeSignatures #-}

-- | Crypto and keys needed for signing transactions.
module Frontend.WalletConnect.Internal
  where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay, tryReadMVar)
import           Control.Exception.Base             (displayException)
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Lens                       hiding ((#))
import           Control.Monad.Fail                 (MonadFail)
import           Control.Newtype.Generics           (Newtype (..))
import           Data.Aeson                         hiding (Object)
import           Data.Bits                          ((.|.))
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as BS
import Data.Maybe (fromMaybe)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import           GHC.Generics                       (Generic)
import           GHCJS.Buffer                       (createFromArrayBuffer, freeze, toByteString)

import           Language.Javascript.JSaddle.Helper (mutableArrayBufferFromJSVal)
import           Language.Javascript.JSaddle.Types  (ghcjsPure)

import           Language.Javascript.JSaddle
                                         hiding ( eval
                                                , jsf
                                                , js
                                                , js0
                                                , jss
                                                , js2
                                                , js3
                                                , jsg2
                                                , jsg
                                                , js1
                                                , (<#)
                                                , (!)
                                                )

import qualified Language.Javascript.JSaddle   as JSaddle
import Reflex.Dom.Core

-- Specialised for Text
eval t = JSaddle.eval (t :: Text)
jsf t = JSaddle.jsf (t :: Text)
js t = JSaddle.js (t :: Text)
jss t = JSaddle.jss (t :: Text)
js0 t = JSaddle.js0 (t :: Text)
js1 t = JSaddle.js1 (t :: Text)
js2 t = JSaddle.js2 (t :: Text)
js3 t = JSaddle.js3 (t :: Text)
jsg t = JSaddle.jsg (t :: Text)
jsg2 t = JSaddle.jsg2 (t :: Text)
valT t = val (t :: Text)
(<#) o t = (JSaddle.<#) o (t :: Text)
(!) o t = (JSaddle.!) o (t :: Text)

logValueF = fun $ \_ _ [value] -> logValue value

logValue value = do
  w <- jsg "console"
  w ^. js1 "log" value
  pure ()

data WalletConnectRequest = WalletConnectRequest
  { _walletConnectRequest_topic :: Text
  , _walletConnectRequest_id :: JSVal
  , _walletConnectRequest_chainId :: Text
  , _walletConnectRequest_method :: Text
  , _walletConnectRequest_params :: JSVal
  }

data WalletConnectResponse = WalletConnectResponse
  { _walletConnectResponse_topic :: Text
  , _walletConnectResponse_id :: JSVal
  , _walletConnectResponse_result :: JSVal
  }

data WalletConnectSession t m = WalletConnectSession
  { _walletConnectSession_active :: Dynamic t Bool
  , _walletConnectSession_request :: Event t WalletConnectRequest
  , _walletConnectSession_respond :: Event t WalletConnectResponse -> m (Event t ())
  }

doInit :: (_)
  => Maybe Text
  -> Text
  -> Event t ()
  -> Event t Text
  -> m (WalletConnectSession t m)
doInit mRelayUrl projectId initEv uriEv = do
  (reqEv, reqAction) <- newTriggerEvent

  (sessionEv, sessionAction) <- newTriggerEvent

  clientMVar <- liftIO $ newEmptyMVar

  performEvent $ ffor initEv $ \_ -> liftJSM $ do
    cPromise <- clientInit mRelayUrl projectId
    cPromise ^. js2 "then" (subscribeToEvents clientMVar reqAction sessionAction) logValueF

  performEvent $ ffor uriEv $ \uri -> liftJSM $ do
    mClient <- liftIO $ tryReadMVar clientMVar
    mapM (doPair uri) mClient

  sessionActive <- holdDyn False sessionEv
  display sessionActive

  let
    respond ev = performEvent $ ffor ev $ \v -> do
      mClient <- liftIO $ tryReadMVar clientMVar
      liftJSM $ mapM_ (doRespond v) mClient

  return $ WalletConnectSession sessionActive reqEv respond

clientInit :: Maybe Text -> Text -> JSM JSVal
clientInit mRelayUrl projectId = do
  wcc <- jsg "WalletConnectClient"
  client <- wcc ! "Client"
  args <- do
    o <- create
    (o <# "logger") ("debug" :: Text)
    -- The default specified in client (relay.wallet-connect.com) does not work
    -- so always specify one here.
    (o <# "relayUrl") (fromMaybe "wss://bridge.walletconnect.org" mRelayUrl)
    (o <# "projectId") projectId
    (o <# "controller") True
    pure o
  client ^. js1 "init" args

-- subscribeToEvents :: JSVal -> JSM ()
subscribeToEvents clientMVar reqAction sessionAction = fun $ \_ _ [client] -> do
  logValue ("subscribeToEvents" :: Text)
  logValue client

  liftIO $ putMVar clientMVar client

  wcc <- jsg "WalletConnectClient"
  events <- wcc ! "CLIENT_EVENTS"
  session <- events ! "session"

  let onProposal = fun $ \_ _ [v] -> do
        logValue "onProposal"
        logValue v
        response <- do
          o <- create
          state <- do
            o <- create
            (o <# "accounts") ["eip155:42:0x8fd00f170fdf3772c5ebdcd90bf257316c69ba45"]
            pure o
          (o <# "state") state
          pure o
        args <- do
          o <- create
          (o <# "proposal") v
          (o <# "response") response
          pure o
        void $ client ^. js1 "approve" args

  proposal <- session ! "proposal"
  client ^. js2 "on" proposal onProposal

  let onRequest = fun $ \_ _ [requestEvent] -> do
        logValue "onRequest"
        logValue requestEvent
        chainId <- valToText =<< requestEvent ! "chainId"
        topic <- valToText =<< requestEvent ! "topic"
        req <- requestEvent ! "request"
        id' <- req ! "id"
        method <- valToText =<< req ! "method"
        params <- req ! "params"
        liftIO $ reqAction (WalletConnectRequest topic id' chainId method params)

  request <- session ! "request"
  client ^. js2 "on" request onRequest

  let onCreate = fun $ \_ _ _ -> do
        logValue "onCreate"
        liftIO $ sessionAction True

  created <- session ! "created"
  client ^. js2 "on" created onCreate

  let onDelete = fun $ \_ _ _ -> do
        logValue "onDelete"
        liftIO $ sessionAction False

  deleted <- session ! "deleted"
  void $ client ^. js2 "on" deleted onDelete

doPair uri client = do
  logValue "doPair"
  logValue uri
  args <- do
    o <- create
    (o <# "uri") uri
    pure o
  void $ client ^. js1 "pair" args

doRespond :: WalletConnectResponse -> JSVal -> JSM ()
doRespond (WalletConnectResponse topic id' result) client = do
  logValue "doRespond"
  logValue topic
  args <- do
    o <- create
    (o <# "topic") topic
    response <- do
      o <- create
      (o <# "result") result
      (o <# "jsonrpc") ("2.0" :: Text)
      (o <# "id") id'
      pure o
    (o <# "response") response
    pure o
  logValue args
  void $ client ^. js1 "respond" args
