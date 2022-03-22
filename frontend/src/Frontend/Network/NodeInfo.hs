{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}


-- | Information about a chainweb like node.
--
--   A node (hostname) can either be a node of a chainweb network in some
--   version with some number of available chains or a single `pact -s` node.
--   This module provides the necessary abstractions to work with such nodes.
--
--   In particular it provides `NodeInfo` which can retrieved from an `Authority`
--   by means of `discoverNode`.
--
module Frontend.Network.NodeInfo
  ( -- * Types & Classes
    ChainId
  , NodeInfo (..)
  , nodeVersion
  , nodeInfoRef
    -- * Discover
  , parseNodeRef
  , discoverNode
    -- * Get node/network information.
  , getChainRefBaseUrl
  , getChainBaseUrl
  , getChains
    -- * More details
  , NodeType (..)
  , ChainwebInfo (..)
  , sortChainIds
  ) where

import           Control.Applicative         ((<|>))
import           Control.Arrow               (right, left)
import           Control.Lens
import           Control.Monad
import           Control.Monad               (void)
import           Control.Monad.Except        (ExceptT (..), MonadError,
                                              runExceptT, throwError)
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Except  (except)
import           Data.Aeson                  (Value)
import qualified Data.Aeson                  as Aeson
import qualified Data.Aeson.Lens             as AL
import           Data.Char                   (isDigit)
import           Data.Default
import           Data.List                   (sortOn)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Language.Javascript.JSaddle (JSM, MonadJSM, liftJSM)
import           Reflex.Dom.Class            (HasJSContext (..))
import           Reflex.Dom.Xhr hiding (newXMLHttpRequestWithError)
import           Safe                        (fromJustNote, maximumMay)
import           Text.URI                    (URI (URI))
import qualified Text.URI                    as URI hiding (uriPath)
import           Text.URI.Lens               as URI
import qualified Text.URI.QQ                 as URI
import           UnliftIO.Async
import           UnliftIO.Exception          (catch)
import           UnliftIO.MVar
import qualified Pact.Types.ChainId          as Pact

import           Common.Network              (ChainId (..), ChainRef (..),
                                              NodeRef (..), parseNodeRef)
import           Frontend.Foundation

-- Imports for newXMLHttpRequestWithError
import Control.Concurrent (forkIO)
import Control.Exception (handle)
import qualified Data.CaseInsensitive as CI
import Data.Default
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as Map
import GHCJS.DOM.XMLHttpRequest
import GHCJS.DOM.Enums
import qualified Language.Javascript.JSaddle.Monad as JS (catch)
import GHCJS.DOM.Types (ToJSString, Blob(..), castTo)
import Language.Javascript.JSaddle.Helper (mutableArrayBufferFromJSVal)
import Foreign.JavaScript.Utils (bsFromMutableArrayBuffer, bsToArrayBuffer)
import Control.Exception (throwIO)

data ChainwebInfo = ChainwebInfo
  { _chainwebInfo_version        :: Text
    -- ^ What chainweb version is running on the node.
  , _chainwebInfo_networkVersion :: Text
    -- ^ What version of the network is running.
  , _chainwebInfo_chainIds :: [ChainId]
    -- ^ What chainIds are on this network.
  }
  deriving (Eq, Ord, Show)


data NodeType =
    NodeType_Pact Text -- ^ A pact -s node with the provided version string.
  | NodeType_Chainweb ChainwebInfo -- ^ A chainweb node.
  deriving (Eq, Ord, Show)

-- | Internally used Uri type, which diverges from URI mostly for a mandatory
-- instead of optional Authority. (Thus we can avoid pointless `Maybe`s or
-- partial functions.)
data NodeUri = NodeUri
  { _nodeUri_scheme    :: URI.RText 'URI.Scheme
  , _nodeUri_authority :: URI.Authority
  }
  deriving (Eq, Ord, Show, Generic)

data NodeInfo = NodeInfo
  { _nodeInfo_baseUri :: NodeUri
  , _nodeInfo_type    :: NodeType
  }
  deriving (Eq, Ord, Show, Generic)

nodeVersion :: NodeInfo -> Text
nodeVersion ni = case _nodeInfo_type ni of
  NodeType_Pact v -> v
  NodeType_Chainweb ci -> _chainwebInfo_networkVersion ci

-- | Retrive the `NodeInfo` for a given host by quering its API.
discoverNode :: forall m. (MonadJSM m, MonadUnliftIO m, HasJSContext m) => NodeRef -> m (Either Text NodeInfo)
discoverNode (NodeRef auth) = do
    httpsReqs <- async $ discoverChainwebOrPact httpsUri
    httpReqs <- async $ discoverChainwebOrPact httpUri

    -- For some http only servers waiting for a https response will take ages
    -- on the other hand we need to prefer chainweb detection over pact -s
    -- detection (as the former is more reliable). Therefore we group them by
    -- protocol and go with the first success result.
    waitSuccess [httpsReqs, httpReqs]

  where
    waitSuccess :: [Async (Either Text NodeInfo)] -> m (Either Text NodeInfo)
    waitSuccess = \case
      [] -> pure $ Left ""
      xs -> do
        (finished, r) <- waitAny xs
        case r of
          Left err -> do
            left ((err <> "\n\n") <>) <$> waitSuccess (filter (/= finished) xs)
          Right success -> pure $ Right success

    httpsUri = uriFromSchemeAuth [URI.scheme|https|]
    httpUri = uriFromSchemeAuth [URI.scheme|http|]

    uriFromSchemeAuth scheme =  NodeUri scheme auth


-- | The node this node info is for.
nodeInfoRef :: NodeInfo -> NodeRef
nodeInfoRef = NodeRef . _nodeUri_authority . _nodeInfo_baseUri


-- | Get a base `URI` from a `NodeUri`.
nodeToBaseUri :: NodeUri -> URI
nodeToBaseUri (NodeUri scheme auth) = URI (Just scheme) (Right auth) Nothing [] Nothing


-- | Base url to use for a particular `ChainRef`.
--
--   Note in case the `ChainRef` provides a `_chainRef_node` we have to run a
--   node detection, so this call might fail in that case.
getChainRefBaseUrl :: MonadJSM m => ChainRef -> Maybe NodeInfo -> m (Either Text URI)
getChainRefBaseUrl (ChainRef mNode chainId) mInfo = do
    jsm <- askJSM
    errInfo <- getInfo `runJSM` jsm
    pure $ right (getChainBaseUrl chainId) errInfo
  where
    getInfo = fromMaybe uInfo <$> traverse discoverNode mNode
    uInfo = maybe (Left "No network node available!") Right mInfo

-- | Base url to use for a particular chain.
--
--   This is the url where you can append /send, /local, /listen ...
getChainBaseUrl :: ChainId -> NodeInfo -> URI
getChainBaseUrl chainId (NodeInfo base nType) =
    nodeToBaseUri base & uriPath .~ getChainBasePath chainId nType


-- | Get a list of available chains.
--
getChains :: NodeInfo -> [Pact.ChainId]
getChains (NodeInfo _ nType) =
  case nType of
    NodeType_Pact _ -> [ Pact.ChainId "0" ]
    NodeType_Chainweb info -> _chainwebInfo_chainIds info


-- | Get the path for a given chain id.
getChainBasePath :: ChainId -> NodeType -> [URI.RText 'URI.PathPiece]
getChainBasePath (ChainId chainId) = buildPath . \case
    NodeType_Pact _
      -> [] -- ["api", "v1"]
    NodeType_Chainweb (ChainwebInfo cwVersion netVersion _)
      -> ["chainweb", cwVersion, netVersion, "chain", chainId, "pact"]
  where
    buildPath = fromJustNote "Building chain base path failed!" . traverse URI.mkPathPiece


-- | Find out whether the given host and scheme are either a Pact or a Chainweb node.
discoverChainwebOrPact :: (MonadJSM m, HasJSContext m, MonadUnliftIO m) => NodeUri -> m (Either Text NodeInfo)
discoverChainwebOrPact uri = do
  (chainwebResp, pactResp) <- discoverChainwebNode uri  `concurrently` discoverPactNode uri
  -- pure $ chainwebResp <|> pactResp
  -- No `Error` instance for Text:
  pure $ left T.pack $ left T.unpack chainwebResp <|> left T.unpack pactResp


-- | Retrieve the `NodeInfo` for a given host by quering its API.
--
--   This function will only succeed for chainweb nodes.
discoverChainwebNode :: (MonadJSM m, HasJSContext m, MonadUnliftIO m) => NodeUri -> m (Either Text NodeInfo)
discoverChainwebNode baseUri = runExceptT $ do
    req <- except $ mkSafeReq $ nodeToBaseUri baseUri & URI.uriPath .~ [ [URI.pathPiece|info|] ]
    resp <- ExceptT . fmap (left tshow) $ runReq req

    when (_xhrResponse_status resp /= 200) $
      throwError $ "Received non 200 status: " <> tshow (_xhrResponse_status resp)

    infoI <- note "Parsing /info json value failed" $
      Aeson.decodeStrict . T.encodeUtf8 <=< _xhrResponse_responseText $ resp

    info <- parseChainwebInfo infoI
    pure $ NodeInfo
      { _nodeInfo_baseUri = baseUri
      , _nodeInfo_type = NodeType_Chainweb info
      }


-- | Find out whether the node could be a valid pact -s node.
--
--   WARNING: The check is pretty basic and could easily confuse a `pact -s` with a chainweb node, when
--   chainweb or pact -s evolve a bit, therefore, always run
--   `discoverChainwebNode` first, which is more reliable.
discoverPactNode :: (MonadJSM m, HasJSContext m, MonadUnliftIO m) => NodeUri -> m (Either Text NodeInfo)
discoverPactNode baseUri = runExceptT $ do
    req <- except $ mkSafeReq $ nodeToBaseUri baseUri & URI.uriPath .~ [ [URI.pathPiece|version|] ]
    resp <- ExceptT . fmap (left tshow) $ runReq req
    when (_xhrResponse_status resp /= 200) $
      throwError $ "Received non 200 status: " <> tshow (_xhrResponse_status resp)
    pure $ NodeInfo
      { _nodeInfo_baseUri = baseUri
      , _nodeInfo_type = NodeType_Pact $ fromMaybe "" $ _xhrResponse_responseText resp
      }


-- | Parse `ChainwebInfo` given a `Value` representing /info .
{- parseChainwebInfo :: forall m. MonadPlus m => Value -> m ChainwebInfo -}
parseChainwebInfo :: forall m. (MonadError Text m) => Value -> m ChainwebInfo
parseChainwebInfo v = do
    chainIdVals <- note "Found no nodeChains" $ v ^? AL.key "nodeChains" . AL._Array
    chainIds <- note "nodeChains were not strings" . fmap sortChainIds . traverse (^? AL._String . to Pact.ChainId) . toList $ chainIdVals
    chainwebVersion <- note "Found no nodeApiVersion" $ v ^? AL.key "nodeApiVersion" . AL._String
    networkVersion <- note "Found no nodeVersion " $ v ^? AL.key "nodeVersion" . AL._String

    pure $ ChainwebInfo
      { _chainwebInfo_version = chainwebVersion
      , _chainwebInfo_networkVersion = networkVersion
      , _chainwebInfo_chainIds = chainIds
      }

-- Note, the chain ids come back from the nodes not sorted. It comes back as
-- "nodeChains":["8","9","4","5","6","7","0","1","2","3"]
-- But sorting these is a little tricky. If we just sort the strings, things will get messy with
-- 10, 11, 12, etc but can we actually assume that these are always numbers and sort them as such?
-- This function goes a little over the top to make sure numbers are sorted at the top
-- and if we get any non number chain ids they are alphanumerically sorted down below
sortChainIds :: [Pact.ChainId] -> [Pact.ChainId]
sortChainIds ids = sortOn chainIdIndex ids
  where
   -- Always pad numbers with zero so they appear before words like "2 words"
   chainIdIndex (Pact.ChainId t) = if (isDigitText t) then T.justifyRight (maxDigitWidth + 1) '0' t else t
   maxDigitWidth = fromMaybe 0 . maximumMay . map T.length . filter isDigitText . map Pact._chainId $ ids
   isDigitText = T.all isDigit

runReq
  :: (HasJSContext m, MonadJSM m, MonadUnliftIO m, IsXhrPayload a)
  => SafeXhrRequest a
  -> m (Either XhrException XhrResponse)
runReq (SafeXhrRequest req) = do
  resp <- newEmptyMVar
  void $ newXMLHttpRequestWithErrorSane req (liftIO . putMVar resp)
  takeMVar resp


-- Sane version of newXMLHttpRequestWithError: Report all errors via callback,
-- including those that are thrown in JS. We return () instead of the
-- XmlHttpRequest as we can't get access to the that object in case of an
-- exception and it does not really make sense to throw it, as we are reporting
-- the error via the callback.
newXMLHttpRequestWithErrorSane
    :: forall m a.
      ( HasJSContext m, MonadJSM m, IsXhrPayload a, MonadUnliftIO m)
    => XhrRequest a
    -- ^ The request to make.
    -> (Either XhrException XhrResponse -> JSM ())
    -- ^ A continuation to be called once a response comes back, or in
    -- case of error.
    -> m ()
newXMLHttpRequestWithErrorSane req cb =
    void (newXMLHttpRequestWithError req cb) `catch` handleException
  where
    handleException :: XhrException -> m ()
    handleException e = liftJSM $ cb $ Left e

{-
   When the port number is >=2^16, calling 'newXMLHttpRequestWithError' fails with
   a javascript exception that doesn't seem catchable in haskell.

  'modern-uri' considers the spec allows those ports, so we validate the request instead.

   Possibly a bug in 'newXMLHttpRequestWithError': https://github.com/reflex-frp/reflex-dom/issues/369
-}
newtype SafeXhrRequest a = SafeXhrRequest (XhrRequest a)

mkSafeReq :: URI -> Either Text (SafeXhrRequest ())
mkSafeReq uri = case uri ^. uriAuthority ^? _Right . authPort of
  Just (Just p) | p > maxPort -> Left $ "invalid port number: " <> tshow p
  _ -> Right $ SafeXhrRequest $ xhrRequest "GET" (URI.render uri) def
  where
    maxPort = 2^(16 :: Word) - 1

-- The below code has been copied from Reflex.Dom.Xhr.* modules as it is not exported

newXMLHttpRequestWithError
    :: (HasJSContext m, MonadJSM m, IsXhrPayload a)
    => XhrRequest a
    -- ^ The request to make.
    -> (Either XhrException XhrResponse -> JSM ())
    -- ^ A continuation to be called once a response comes back, or in
    -- case of error.
    -> m XMLHttpRequest
    -- ^ The XHR request, which could for example be aborted.
newXMLHttpRequestWithError req cb = do
  xhr <- xmlHttpRequestNew
  ctx <- askJSM
  void $ liftIO $ forkIO $ handle ((`runJSM` ctx) . cb . Left) $ void . (`runJSM` ctx) $ do
    let c = _xhrRequest_config req
        rt = _xhrRequestConfig_responseType c
        creds = _xhrRequestConfig_withCredentials c
    xmlHttpRequestOpen
      xhr
      (_xhrRequest_method req)
      (_xhrRequest_url req)
      True
      (fromMaybe "" $ _xhrRequestConfig_user c)
      (fromMaybe "" $ _xhrRequestConfig_password c)
    iforM_ (_xhrRequestConfig_headers c) $ xmlHttpRequestSetRequestHeader xhr
    maybe (return ()) (setResponseType xhr . fromResponseType) rt
    setWithCredentials xhr creds
    _ <- xmlHttpRequestOnreadystatechange xhr $ do
      readyState <- xmlHttpRequestGetReadyState xhr
      status <- xmlHttpRequestGetStatus xhr
      statusText <- xmlHttpRequestGetStatusText xhr
      when (readyState == 4) $ do
        t <- if rt == Just XhrResponseType_Text || isNothing rt
             then xmlHttpRequestGetResponseText xhr
             else return Nothing
        r <- xmlHttpRequestGetResponse xhr
        h <- case _xhrRequestConfig_responseHeaders c of
          AllHeaders -> parseAllHeadersString <$>
            getAllResponseHeaders xhr
          OnlyHeaders xs -> traverse (xmlHttpRequestGetResponseHeader xhr)
            (Map.fromSet CI.original xs)
        _ <- liftJSM $ cb $ Right
             XhrResponse { _xhrResponse_status = status
                         , _xhrResponse_statusText = statusText
                         , _xhrResponse_response = r
                         , _xhrResponse_responseText = t
                         , _xhrResponse_headers = h
                         }
        return ()
    _ <- xmlHttpRequestSend xhr (_xhrRequestConfig_sendData c)
    return ()
  return xhr

fromResponseType :: XhrResponseType -> XMLHttpRequestResponseType
fromResponseType XhrResponseType_Default = XMLHttpRequestResponseType
fromResponseType XhrResponseType_ArrayBuffer = XMLHttpRequestResponseTypeArraybuffer
fromResponseType XhrResponseType_Blob = XMLHttpRequestResponseTypeBlob
fromResponseType XhrResponseType_Text = XMLHttpRequestResponseTypeText

xmlHttpRequestGetResponse :: MonadJSM m => XMLHttpRequest -> m (Maybe XhrResponseBody)
xmlHttpRequestGetResponse xhr = do
  mr <- getResponse xhr
  rt <- xmlHttpRequestGetResponseType xhr
  case rt of
       Just XhrResponseType_Blob -> fmap XhrResponseBody_Blob <$> castTo Blob mr
       Just XhrResponseType_Text -> Just . XhrResponseBody_Text <$> xmlHttpRequestGetStatusText xhr
       Just XhrResponseType_Default -> Just . XhrResponseBody_Text <$> xmlHttpRequestGetStatusText xhr
       Just XhrResponseType_ArrayBuffer -> do
           ab <- liftJSM $ mutableArrayBufferFromJSVal mr
           Just . XhrResponseBody_ArrayBuffer <$> bsFromMutableArrayBuffer ab
       _ -> return Nothing

parseAllHeadersString :: Text -> Map (CI.CI Text) Text
parseAllHeadersString s = Map.fromList $ fmap (stripBoth . T.span (/=':')) $
  L.dropWhileEnd T.null $ T.splitOn (T.pack "\r\n") s
  where stripBoth (txt1, txt2) = (CI.mk $ T.strip txt1, T.strip $ T.drop 1 txt2)

xmlHttpRequestSend :: IsXhrPayload payload => XMLHttpRequest -> payload -> JSM ()
xmlHttpRequestSend self p = sendXhrPayload self p `JS.catch` (liftIO . throwIO . convertException)

xmlHttpRequestGetResponseType :: MonadJSM m => XMLHttpRequest -> m (Maybe XhrResponseType)
xmlHttpRequestGetResponseType = fmap toResponseType . getResponseType

xmlHttpRequestGetResponseHeader :: (ToJSString header, MonadJSM m)
                                => XMLHttpRequest -> header -> m Text
xmlHttpRequestGetResponseHeader self header = fromMaybe "" <$> getResponseHeader self header

convertException :: XHRError -> XhrException
convertException e = case e of
  XHRError -> XhrException_Error
  XHRAborted -> XhrException_Aborted

toResponseType :: XMLHttpRequestResponseType -> Maybe XhrResponseType
toResponseType XMLHttpRequestResponseType = Just XhrResponseType_Default
toResponseType XMLHttpRequestResponseTypeArraybuffer = Just XhrResponseType_ArrayBuffer
toResponseType XMLHttpRequestResponseTypeBlob = Just XhrResponseType_Blob
toResponseType XMLHttpRequestResponseTypeText = Just XhrResponseType_Text
toResponseType _ = Nothing
