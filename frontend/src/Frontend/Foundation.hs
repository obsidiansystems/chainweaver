{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE ExtendedDefaultRules   #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Definitions common to the whole frontend.
--
--   And commonly used imports.

module Frontend.Foundation
  ( -- * Useful types
    ReflexValue
  , MDynamic
  , LeftmostEv (..)
  , AppCfg(..)
    -- * Lenses and Prisms
  , makePactLenses
  , makePactLensesNonClassy
  , makePactPrisms
    -- * Helpers that should really not be here
  , getBrowserProperty
    -- * Common Foundation
  , module Common
    -- * Re-exports
  , module Reflex.Extended
  , module Reflex.Network.Extended
  , module Language.Javascript.JSaddle
  , module Obelisk.Configs
  , module Reflex.Dom.Contrib.CssClass
  , forkJSM
  ) where

import           Control.Lens
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.Coerce                       (coerce)
import           Data.Foldable
import           Data.Semigroup
import           Data.Text                         (Text)
import           GHC.Generics                      (Generic)
import           Language.Haskell.TH               (DecsQ)
import           Language.Haskell.TH.Syntax        (Name)
import           Language.Javascript.JSaddle       (JSM, MonadJSM, askJSM,
                                                    liftJSM, runJSM)
import qualified Language.Javascript.JSaddle       as JS
import           Language.Javascript.JSaddle.Monad (JSContextRef)
import           Obelisk.Configs
import           Reflex.Dom.Class                  (HasJSContext (..),
                                                    JSContextSingleton (..))
import           Reflex.Dom.Contrib.CssClass
import           Reflex.Dom.WebSocket              (forkJSM)
import           Reflex.Extended
import           Reflex.Network.Extended


import           Data.Maybe

import           Common.Foundation                 as Common

data AppCfg t m = AppCfg
  { _appCfg_gistEnabled :: Bool
  , _appCfg_externalFileOpened :: Event t Text
  -- ^ File contents from file chosen in "open file" dialog
  , _appCfg_openFileDialog :: JSM ()
  -- ^ Trigger an "open file" dialog
  , _appCfg_loadEditor :: m (Maybe Text)
  -- ^ Initial code to load into editor
  , _appCfg_editorReadOnly :: Bool
  -- ^ Is the editor read only?
  }

-- | Shorthand for Dynamic t (Maybe a).
--
--   Usually used for things that might not be loaded yet.
type MDynamic t a = Dynamic t (Maybe a)

-- | Wrapper around Event with a Monoid instance based on `leftmost`.
newtype LeftmostEv t a = LeftmostEv
  { unLeftmostEv :: Event t a
  }

instance Reflex t => Semigroup (LeftmostEv t a) where
  (LeftmostEv a) <> (LeftmostEv b) = LeftmostEv $ leftmost [a, b]

instance Reflex t => Monoid (LeftmostEv t a) where
  mempty = LeftmostEv never
  mappend = (<>)
  mconcat = LeftmostEv . leftmost . coerce

-- | Lenses in this project should be generated by means of this function.
--
--   We generate lazy classy lenses. Classes make the export lists less tedious
--   and allows for generic code, which will come in handy when the project
--   grows.
--
--   We want lazy lenses so we can uses lenses also in recursive definitions.

makePactLenses :: Name -> DecsQ
makePactLenses =
  makeLensesWith
    ( classyRules
        & generateLazyPatterns .~ True
        & createClass .~ True
    )

-- | Non classy non simple lenses.
--
--   For some reason ( I have not investigated yet ), classy non simple lenses
--   don't seem to work properly, at least not if the type has parameters.
--   Therefore if you have a type where you need non simple lenses (lenses that
--   can change the type), you have to use this function instead of
--   `makePactLenses`.
makePactLensesNonClassy :: Name -> DecsQ
makePactLensesNonClassy =
  makeLensesWith
    ( lensRules
        & simpleLenses .~ False
        & generateLazyPatterns .~ True
    )

-- | Make Prisms in "pact style".
--
--   Currently this is just standard `makePrisms`
makePactPrisms :: Name -> DecsQ
makePactPrisms = makePrisms

getBrowserProperty :: forall m. MonadJSM m => Text -> m Bool
getBrowserProperty property = liftJSM $ fromMaybe False <$> JS.catch (JS.fromJSVal =<< JS.eval ("bowser." <> property)) (\(_ :: JS.JSException) -> pure Nothing)

-- TODO: upstream this?
instance HasJSContext JSM where
  type JSContextPhantom JSM = JSContextRef
  askJSContext = JSContextSingleton <$> askJSM


-- | Re-use data constructors more flexibly.
type family ReflexValue (f :: * -> *) x where
    ReflexValue (Dynamic t) x = Dynamic t x

    ReflexValue Identity x = x

    ReflexValue (Behavior t) x = Behavior t x

    ReflexValue (Event t) x = Event t x
