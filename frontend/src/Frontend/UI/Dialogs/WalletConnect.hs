{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Frontend.UI.Dialogs.WalletConnect where

import Control.Monad (join, void, forM_)
-- import Control.Monad.IO.Class
-- import Control.Monad.Fix
-- import Control.Lens
-- import qualified Data.Aeson as A
-- import Data.Maybe (listToMaybe, fromMaybe)
-- import qualified Data.Map as M
-- import qualified Data.Set as Set
-- import Data.Text (Text)
-- import qualified Data.Text as T
import Reflex.Dom hiding (Request)
-- import Pact.Server.ApiClient (runTransactionLoggerT, noLogger)
-- import Obelisk.Frontend
-- import Obelisk.Route.Frontend
-- import Obelisk.Generated.Static
-- import System.IO
import Language.Javascript.JSaddle (valToJSON, liftJSM, JSM, fun, jsg, js0)


import WalletConnect.Wallet
import Frontend.AppCfg
import           Frontend.UI.Modal
import           Frontend.UI.Widgets
import           Frontend.UI.Widgets.Helpers (dialogSectionHeading)
import Common.Wallet

uiWalletConnect
  :: ( MonadWidget t m
     , Monoid mConf
     )
  => WalletConnect t
  -> Event t ()
  -> m (mConf, Event t ())
uiWalletConnect wc@(WalletConnect pairings sessions _ _ _) _ = do
  onClose <- modalHeader $ text "Wallet Connect"

  modalMain $ mdo
    selectEv <- uiSegment mempty $ do
      uiGroupHeader mempty $
        dialogSectionHeading mempty "Active Sessions"

      dyn $ ffor sessions $ \ss -> do
        forM_ ss $ \session -> uiGroup "segment" $ do
          let m = snd $ _session_peer session
          showMetaData m
          ev <- uiButton btnCfgTertiary $ text "Disconnect"
          performEvent $ ffor ev $ \_ -> liftJSM $ _session_disconnect session
    pure ()

  pure (mempty, onClose)
  where
    showMetaData m = do
      uiGroupHeader mempty $ text $ _metadata_name m
      el "p" $ text $ _metadata_url m
      el "p" $ text $ _metadata_description m
