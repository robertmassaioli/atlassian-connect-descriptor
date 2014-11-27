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

-- | Every Atlassian Connect add-on can be installed, uninstalled, enabled and disabled. These are known as 'Lifecycle'
-- events. These events will fire on each and every Cloud instance that your add-on is installed on. You can request in
-- your Atlassian Connect add-on descriptor to be alerted of lifecycle events. When the event fires, if you have
-- requested it, you will be given the details of the event in a JSON blob by the host application.
--
-- The lifecycle events are documented fully in the Atlassian Connect documentation:
-- <https://developer.atlassian.com/static/connect/docs/modules/lifecycle.html>
--
-- It is important to note that the installed event is particularily important to any Atlassian Connect add-on that
-- needs to use 'Jwt' auth tokens because the installed handler will come with the shared secret for your add-on on
-- that particular instance.
data Lifecycle = Lifecycle
   { installed   :: Maybe NU.URI -- ^ Potential relative URI to call every time an add-on is installed on an instance.
   , uninstalled :: Maybe NU.URI -- ^ Potential relative URI to call every time an add-on is uninstalled on an instance.
   , enabled     :: Maybe NU.URI -- ^ Potential relative URI to call every time an add-on is enabled on an instance.
   , disabled    :: Maybe NU.URI -- ^ Potential relative URI to call every time an add-on is disabled on an instance.
   } deriving (Show, Generic) -- TODO

instance ToJSON Lifecycle where
   toJSON = genericToJSON baseOptions

-- | The empty 'Lifecycle' allowing you to specify exactly which events you wish to handle with Haskell record syntax.
emptyLifecycle :: Lifecycle
emptyLifecycle = Lifecycle Nothing Nothing Nothing Nothing

-- | The default 'Lifecycle' where installed goes to /installed and so on and so forth for every lifecycle event. You
-- can choose to disclude certain events by 'Nothing' them out.
defaultLifecycle :: Lifecycle
defaultLifecycle = Lifecycle
   { installed = NU.parseRelativeReference "/installed"
   , uninstalled = NU.parseRelativeReference "/uninstalled"
   , enabled = NU.parseRelativeReference "/enabled"
   , disabled = NU.parseRelativeReference "/disabled"
   }
