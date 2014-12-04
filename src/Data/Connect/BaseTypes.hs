{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.Connect.BaseTypes where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Connect.AesonHelpers
import           Data.Connect.OrphanInstances ()
import           Data.Text
import qualified Data.Time.Units              as DTU
import           GHC.Generics
import qualified Network.URI                  as NU

-- | This data type represents a Key for a particular data type.
data Key t a = Key t deriving (Show, Eq, Generic)

-- | This data type represents an Atlassian Connect Add-on key.
data PluginKey = PluginKey Text deriving (Show, Eq, Generic)

-- | Represents a timeout in seconds.
newtype Timeout = Timeout DTU.Second deriving (Show, Eq, Enum, Num, Ord, Real, Integral)

-- | Represents the Vendor of the add-on; which will be you. Put your details in this structure.
data Vendor = Vendor
   { vendorName :: Name Vendor -- ^ Your name as a Vendor. Might be your personal name or your business name.
   , vendorUrl :: NU.URI -- ^ A URL to a website that represents you as a vendor.
   } deriving (Show, Eq, Generic)

-- | If your Atlassian Connect addon wants to perform any server side communication with the host product then you will
-- need to use authentication. Otherwise you should specify that you don't need authentication.
data Authentication = Authentication
   { authType :: AuthType -- ^ The authentication type that you wish to use.
   } deriving (Show, Eq, Generic)

-- | The authentication type that you wish to use in your Add-on.
data AuthType
   = Jwt -- ^ If you need to communicate with the host product then you will want to request JWT authentication.
   | None -- ^ If you do not need to communicate the host product then you should request None for authentication.
   deriving (Show, Eq, Generic)

-- | Represents an arbitrary icon. Potentially for an Atlassian Connect module or for the entire add-on itself.
data IconDetails = IconDetails
   { iconUrl    :: Text -- ^ The URI to the icon.
   , iconWidth  :: Maybe Integer -- ^ The width of the icon.
   , iconHeight :: Maybe Integer -- ^ The height of the icon.
   } deriving (Show, Generic)

instance ToJSON PluginKey

-- | Atlassian Connect descriptors contain many names: module names, add-on names, vendor names etc. We want to make sure
-- that these names don't get put in places that they do not belong. Or, if they do get moved around, they get moved around
-- specifically. We are just adding type saefty to names.
data Name a = Name Text deriving (Show, Eq, Generic)

instance ToJSON (Name PluginKey)
instance ToJSON (Name Vendor)

instance ToJSON IconDetails where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "icon"
      }

instance ToJSON Vendor where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "vendor"
      }

instance ToJSON Authentication where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "auth"
      }

instance ToJSON AuthType where
   toJSON Jwt  = "jwt"
   toJSON None  = "none"

data Length
   = Pixels Integer
   | Percentage Integer
   deriving (Show, Generic)

instance ToJSON Length where
   toJSON (Pixels x) = String . pack $ (show x ++ "px")
   toJSON (Percentage x) = String . pack $ (show x ++ "%")