{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE QuasiQuotes #-}
-- |
-- Copyright   :  (C) 2020 Kadena
-- License     :  BSD-style (see the file LICENSE)
--
module Frontend.UI.Dialogs.TxLogs
  ( uiTxLogs
  ) where

import Control.Lens -- (iforM_, (^.), _head)
import Control.Monad (void)

import Reflex
import Reflex.Dom.Core

import qualified Data.Text as Text

import Data.Either (isRight)
import Data.Time (formatTime)

import Common.Wallet (PublicKey, textToKey, Key (..), KeyPair (..))
import Frontend.AppCfg (FileFFI (..))
import Frontend.Wallet (HasWallet (..))
import Frontend.UI.Modal (modalHeader, modalFooter)
import Frontend.Foundation
import Frontend.UI.Widgets

import qualified Pact.Types.Command as Pact
import qualified Pact.Types.Util as Pact
import qualified Pact.Types.Hash as Pact
import qualified Pact.Types.ChainId as Pact

import qualified Pact.Server.ApiClient as Api

type HasUiTxLogModelCfg model mConf key m t =
  ( Monoid mConf
  , Flattenable mConf t
  , HasWallet model key t
  , Api.HasTransactionLogger m
  )

uiTxLogs
  :: ( MonadWidget t m
     , HasUiTxLogModelCfg model mConf key m t
     )
  => FileFFI t m
  -> model
  -> Event t ()
  -> m (mConf, Event t ())
uiTxLogs fileFFI model _onExtClose = do
  txLogger <- Api.askTransactionLogger
  pb <- getPostBuild

  let dKey = fmap (preview (ix 0 . to _key_pair . to _keyPair_publicKey))
        $ model ^. wallet_keys

  onClose <- modalHeader $ text "Transaction Log"
  onLogLoad <- performEvent $ liftIO (Api._transactionLogger_loadFirstNLogs txLogger 10) <$ pb

  divClass "modal__main key-details" $ do
    let
      mkCol w = elAttr "col" ("style" =: ("width: " <> w)) blank
      mkHeading = elClass "th" "table__heading" . text
      td extraClass = elClass "td" ("table__cell " <> extraClass)
      tableAttrs = "class" =: "tx-logs table"

    void $ runWithReplace blank $ ffor onLogLoad $ \case
      Left e -> divClass "tx-log__decode-error" $ text $ Text.pack e
      Right (timeLocale, cmdLogs) -> elAttr "table" tableAttrs $ do
        -- Structure
        el "colgroup" $ do
          mkCol "10%"
          mkCol "40%"
          mkCol "20%"
          mkCol "20%"
          mkCol "10%"
        -- Headings
        el "thead" $ el "tr" $ traverse_ mkHeading $
          [ ""
          , "Creation Time"
          , "Sender"
          , "Chain ID"
          , "Request Key"
          ]

        -- Rows
        el "tbody" $ iforM_ cmdLogs $ \i cmdLog -> elClass "tr" "table-row" $ do
          td "tx-log-row__ix" $ text $ Text.justifyRight 2 '0' $ Pact.tShow i

          -- Expected time format : YYYY-MM-DD  HH:MM
          td "tx-log-row__timestamp" $ text $ Text.pack
            $ formatTime timeLocale "%F %R" $ Api._commandLog_timestamp cmdLog

          let sender = Api._commandLog_sender cmdLog
          td "tx-log-row__sender" $ case (textToKey sender) :: Maybe PublicKey of
            Nothing -> text sender
            Just _ -> text $ Text.take 8 sender

          td "tx-log-row__chain" $ text
            $ Pact._chainId $ Api._commandLog_chain cmdLog

          td "tx-log-row__request-key" $ elClass "span" "request-key-text" $ text
            $ Pact.hashToText $ Pact.unRequestKey $ Api._commandLog_requestKey cmdLog

  -- Both export wallet and txnlog export use the first 8 characters of the first
  -- key in the wallet. Given that key is deterministic, could we simply generate it
  -- rather than either loading keys or requiring them to exist in the wallet first?
  _ <- modalFooter $ dyn_ $ ffor dKey $ \case
    Nothing -> void $ cancelButton
      (def & uiButtonCfg_disabled .~ constDyn True)
      "Export Unavailable"

    Just pk ->
      exportButton txLogger pk

  pure (mempty, onClose)
  where
    exportButton txlog pk = mdo
      (onFileErr, onContentsReady) <- fmap fanEither $ performEvent $
        liftIO (Api._transactionLogger_exportFile txlog pk) <$ onClick

      onDeliveredFileOk <- _fileFFI_deliverFile fileFFI onContentsReady

      status <- holdDyn "fa-download" $ leftmost
        [ "tx-log-export-fail fa-times" <$ traceEvent "file err" onFileErr
        , "tx-log-export-success fa-check" <$ ffilter isRight (traceEvent "delivered file" onDeliveredFileOk)
        ]

      onClick <- uiButtonDyn btnCfgPrimary $ do
        text "Export Full Transaction Log"
        elDynClass "i" ("fa fa-fw tx-log-export-status " <> status) blank

      pure ()
