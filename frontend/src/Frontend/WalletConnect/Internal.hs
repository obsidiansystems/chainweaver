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
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Frontend.WalletConnect.Internal
  ( doInit
  , PeerMetadata(..)
  , Proposal(..)
  , Request(..)
  , Session(..)
  , WalletConnect(..)
  )
  where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, newMVar, putMVar, takeMVar, threadDelay, readMVar)
import           Control.Exception.Base             (displayException)
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Lens                       hiding ((#))
import           Control.Monad.Fail                 (MonadFail)
import           Control.Newtype.Generics           (Newtype (..))
-- import           Data.Aeson                         hiding (Object, Value)
import qualified Data.Aeson as Aeson
import           Data.Bits                          ((.|.))
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as BS
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as M
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
import Reflex.Dom.Core hiding (Request)

type Account = Text
type Chain = Text
type Method = Text
type Topic = Text
type PublicKey = Text

data PeerMetadata = PeerMetadata
  { _peerMetadata_name :: Text
  , _peerMetadata_url :: Text
  , _peerMetadata_icons :: [Text]
  , _peerMetadata_description :: Text
  }
  deriving (Show)

data Proposal = Proposal
  { _proposal_topic :: Topic
  , _proposal_proposer :: (PublicKey, PeerMetadata)
  , _proposal_permissions :: ([Chain], [Method])
  , _proposal_approval :: (Either () [Account] -> JSM ())
  }

data Request = Request
  { _request_chainId :: Chain
  , _request_method :: Method
  , _request_params :: Aeson.Value
  }

data Session = Session
  { _session_topic :: Topic
  , _session_disconnect :: JSM ()
  , _session_peer :: (PublicKey, PeerMetadata)
  }

data WalletConnect t = WalletConnect
  { _walletConnect_sessions :: Dynamic t (Map Topic Session)
  , _walletConnect_proposals :: Event t Proposal
  , _walletConnect_requests :: Event t (Topic, Request, Either () Aeson.Value -> JSM ())
  , _walletConnect_pair :: Text -> IO ()
  }

doInit :: (_)
  => Maybe Text -- Relay URL
  -> Text       -- Project Id
  -> m (WalletConnect t)
doInit mRelayUrl projectId = do
  (uriEv, uriAction) <- newTriggerEvent
  (reqEv, reqAction) <- newTriggerEvent
  (sessionEv, sessionAction) <- newTriggerEvent
  (proposalEv, proposalAction) <- newTriggerEvent

  clientMVar <- liftIO $ newEmptyMVar

  liftJSM $ do
    clientPromise <- clientInit mRelayUrl projectId
    clientPromise ^. js2 "then"
      (subscribeToEvents clientMVar reqAction proposalAction sessionAction)
      logValueF -- TODO: handle errors

  performEvent $ ffor uriEv $ \uri -> liftJSM $ do
    client <- liftIO $ readMVar clientMVar -- TODO: timeout, Report error
    doPair uri client

  rec
    sessions <- widgetHold (pure mempty) $ ffor (attach (current sessions) sessionEv) $ \(old, new) -> do
      client <- liftIO $ readMVar clientMVar
      (M.fromList <$>) $ forM new $ \(t, s) ->
        case M.lookup t old of
          Just o -> pure (t,o)
          Nothing -> (t,) <$> makeSession client t s

  return $ WalletConnect sessions proposalEv reqEv uriAction

makeSession :: (MonadJSM m) => JSVal -> Text -> JSVal -> m Session
makeSession client topic session = do
  liftJSM $ do
    logValue "makeSession"
    logValue session
  metadata <- liftJSM $ getMetadata =<< session ! "peer"
  let
    delete = do
      logValue $ "doing disconnect of " <> topic
      args <- do
        o <- create
        (o <# "topic") topic
        (o <# "reason") ("USER_DISCONNECTED" :: Text) -- todo
        pure o
      void $ client ^. js1 "disconnect" args

  return $ Session topic delete metadata

getMetadata :: (MonadJSM m) => JSVal -> m (PublicKey, PeerMetadata)
getMetadata v = liftJSM $ do
  pk <- valToText =<< v ! "publicKey"
  m <- v ! "metadata"
  n <- valToText =<< m ! "name"
  u <- valToText =<< m ! "url"
  i <- fromJSValUncheckedListOf =<< m ! "icons"
  d <- valToText =<< m ! "description"
  return $ (pk, PeerMetadata n u i d)

clientInit :: Maybe Text -> Text -> JSM JSVal
clientInit mRelayUrl projectId = do
  wcc <- jsg "WalletConnectClient"
  client <- wcc ! "Client"
  args <- do
    o <- create
    (o <# "logger") ("debug" :: Text)
    -- The default specified in client (relay.wallet-connect.com) does not work
    -- so always specify one here.
    (o <# "relayUrl") (fromMaybe "wss://relay.walletconnect.org" mRelayUrl)
    (o <# "projectId") projectId
    (o <# "controller") True
    pure o
  client ^. js1 "init" args

subscribeToEvents clientMVar reqAction proposalAction sessionAction = fun $ \_ _ [client] -> do
  logValue ("subscribeToEvents" :: Text)
  logValue client

  liftIO $ putMVar clientMVar client

  wcc <- jsg "WalletConnectClient"
  events <- wcc ! "CLIENT_EVENTS"
  session <- events ! "session"

  let
    onProposal = fun $ \_ _ [proposal] -> do
      logValue "onProposal"
      logValue proposal
      topic <- valToText =<< proposal ! "topic"
      permissions <- do
        p <- proposal ! "permissions"
        b <- p ! "blockchain"
        chains <- fromJSValUncheckedListOf =<< b ! "chains"
        j <- p ! "jsonrpc"
        methods <- fromJSValUncheckedListOf =<< j ! "methods"
        return (chains, methods)
      proposer <- do
        getMetadata =<< proposal ! "proposer"
      liftIO $ proposalAction $ Proposal topic proposer permissions (either (doReject proposal) (doApprove proposal))

    doReject proposal _ = do
      args <- do
        o <- create
        (o <# "proposal") proposal
        (o <# "reason") "NOT_APPROVED"
        pure o
      void $ client ^. js1 "reject" args

    doApprove proposal accounts = do
      response <- do
        o <- create
        state <- do
          o <- create
          (o <# "accounts") accounts
          pure o
        (o <# "state") state
        pure o
      args <- do
        o <- create
        (o <# "proposal") proposal
        (o <# "response") response
        pure o
      void $ client ^. js1 "approve" args

  proposal <- session ! "proposal"
  client ^. js2 "on" proposal onProposal

  let
    onRequest = fun $ \_ _ [requestEvent] -> do
      logValue "onRequest"
      logValue requestEvent
      topic <- valToText =<< requestEvent ! "topic"
      chainId <- valToText =<< requestEvent ! "chainId"
      req <- requestEvent ! "request"
      id' <- req ! "id"
      method <- valToText =<< req ! "method"
      params <- fromJSValUnchecked =<< req ! "params"
      let
        doSend v = do
          result <- mapM toJSVal v
          doRespond client topic id' result

      liftIO $ reqAction
        ( topic
        , Request chainId method params
        , doSend)

  request <- session ! "request"
  client ^. js2 "on" request onRequest

  let onSync = fun $ \_ _ _ -> do
        logValue "onSync"
        s <- client ! "session"
        v <- fromJSValUncheckedListOf =<< s ! "values"
        tp <- forM v $ \session -> do
          t <- valToText =<< session ! "topic"
          pure (t, session)
        liftIO $ sessionAction tp

  sync <- session ! "sync"
  void $ client ^. js2 "on" sync onSync

doPair uri client = do
  logValue "doPair"
  logValue uri
  args <- do
    o <- create
    (o <# "uri") uri
    pure o
  void $ client ^. js1 "pair" args

doRespond :: JSVal -> Topic -> JSVal -> Either () JSVal -> JSM ()
doRespond client topic id' result = do
  logValue "doRespond"
  logValue topic
  args <- do
    o <- create
    (o <# "topic") topic
    response <- do
      o <- create
      case result of
        Left _ -> (o <# "error") ("JSONRPC_REQUEST_METHOD_REJECTED" :: Text)
        Right v -> (o <# "result") v
      (o <# "jsonrpc") ("2.0" :: Text)
      (o <# "id") id'
      pure o
    (o <# "response") response
    pure o
  logValue args
  void $ client ^. js1 "respond" args

-- JSaddle APIs

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
