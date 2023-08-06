{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.Connect.BaseTypes
   ( Key(..)
   , PluginKey(..)
   , Timeout(..)
   , Vendor(..)
   , Authentication(..)
   , AuthType(..)
   , IconDetails(..)
   , Name(..)
   , I18nText(..)
   , simpleText
   , URLBean(..)
   , toUrl
   , Length(..)
   )
   where

import qualified Data.Aeson                   as DA
import qualified Data.Aeson.Types             as DAT
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

instance DA.ToJSON PluginKey

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

-- | Atlassian Connect descriptors contain many names: module names, add-on names, vendor names etc. We want to make sure
-- that these names don't get put in places that they do not belong. Or, if they do get moved around, they get moved around
-- specifically. We are just adding type saefty to names.
data Name a = Name Text deriving (Show, Eq, Generic)

-- | Represents a standard text type in the descriptor than may be Internationalised in the future. However, currently
-- there is no I18n support: <http://goo.gl/9vJEsW>
data I18nText = I18nText
   { dValue :: Text -- ^ The raw text value that will show.
   , dI18n  :: Maybe Text -- ^ The potential i18n key that will be used when we eventually have I18n support: <http://goo.gl/9vJEsW>
   } deriving (Show, Generic)

instance DA.ToJSON I18nText where
   toJSON = DAT.genericToJSON baseOptions
      { DA.fieldLabelModifier = stripFieldNamePrefix "d"
      }

-- | Since there is currently no I18n support (<http://goo.gl/9vJEsW>) we have this helper method to quickly create an 'I18nText' from a standard
-- 'Text' object.
simpleText :: Text -> I18nText
simpleText t = I18nText { dValue = t, dI18n = Nothing }

-- | This represents a URL wrapped as an object instead of as a plain text element.
data URLBean = URLBean
   { ubUrl :: Text -- ^ The raw URL.
   } deriving (Show, Generic)

instance DA.ToJSON URLBean where
   toJSON = DAT.genericToJSON baseOptions
         { DA.fieldLabelModifier = stripFieldNamePrefix "ub"
         }

-- | Wrap a regular 'Text' based URL inside a URLBean.
toUrl :: Text -> URLBean
toUrl = URLBean

instance DA.ToJSON (Name PluginKey)
instance DA.ToJSON (Name Vendor)

instance DA.ToJSON IconDetails where
   toJSON = DAT.genericToJSON baseOptions
      { DA.fieldLabelModifier = stripFieldNamePrefix "icon"
      }

instance DA.ToJSON Vendor where
   toJSON = DAT.genericToJSON baseOptions
      { DA.fieldLabelModifier = stripFieldNamePrefix "vendor"
      }

instance DA.ToJSON Authentication where
   toJSON = DAT.genericToJSON baseOptions
      { DA.fieldLabelModifier = stripFieldNamePrefix "auth"
      }

instance DA.ToJSON AuthType where
   toJSON Jwt  = "jwt"
   toJSON None  = "none"

-- | A basic length type for HTML elements. Useful for 'WebPanel's and other modules that may require length specifications.
data Length
   = Pixels Integer -- ^ Specify a length in pixels
   | Percentage Integer -- ^ Specify a length as a percentage in the range [0-100].
   deriving (Show, Generic)

instance DA.ToJSON Length where
   toJSON (Pixels x) = DAT.String . pack $ (show x ++ "px")
   toJSON (Percentage x) = DAT.String . pack $ (show x ++ "%")