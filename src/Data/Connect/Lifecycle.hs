{-# LANGUAGE DeriveGeneric #-}
module Data.Connect.Lifecycle
    ( Lifecycle(..)
    , emptyLifecycle
    , defaultLifecycle
    ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Connect.AesonHelpers
import           Data.Connect.OrphanInstances
import           GHC.Generics
import qualified Network.URI                  as NU

data Lifecycle = Lifecycle
   { installed   :: Maybe NU.URI
   , uninstalled :: Maybe NU.URI
   , enabled     :: Maybe NU.URI
   , disabled    :: Maybe NU.URI
   } deriving (Show, Generic) -- TODO

instance ToJSON Lifecycle where
   toJSON = genericToJSON baseOptions

emptyLifecycle :: Lifecycle
emptyLifecycle = Lifecycle Nothing Nothing Nothing Nothing

defaultLifecycle :: Lifecycle
defaultLifecycle = Lifecycle
   { installed = NU.parseRelativeReference "/installed"
   , uninstalled = NU.parseRelativeReference "/uninstalled"
   , enabled = NU.parseRelativeReference "/enabled"
   , disabled = NU.parseRelativeReference "/disabled"
   }
