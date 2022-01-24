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

type Topic = Text

data WalletConnectSession t m = WalletConnectSession
  { _walletConnectSession_active :: Dynamic t Bool
  , _walletConnectSession_request :: Event t (Text, Text, Text)
  , _walletConnectSession_respond :: Event t (Topic, Text) -> m (Event t ())
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
  requests <- foldDyn (:) [] reqEv
  display requests
  return $ WalletConnectSession sessionActive reqEv (\_ -> pure never)

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
        j <- jsg "JSON"
        c <- valToText =<< requestEvent ! "chainId"
        t <- valToText =<< requestEvent ! "topic"
        r <- requestEvent ! "request"
        req <- valToText =<< j ^. js1  "stringify" r
        liftIO $ reqAction (c, t, req)

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
  pure ()
