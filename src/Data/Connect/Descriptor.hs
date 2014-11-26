{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- TODO the descriptor needs to be extracted into more modules and perhaps its own library

-- TODO be more selective about what we export from here
module Data.Connect.Descriptor where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Connect.AesonHelpers
import           Data.Connect.Conditions
import           Data.Connect.Lifecycle
import           Data.Connect.Scopes
import           Data.Connect.Webhooks
import           Data.Text
import           Data.Time.Units
import           GHC.Generics
import           Network.URI

data Plugin = Plugin
   { pluginName        :: Maybe (Name Plugin)
   , pluginDescription :: Maybe Text
   , pluginKey         :: PluginKey
   , pluginBaseUrl     :: URI
   , vendor            :: Maybe Vendor
   , authentication    :: Authentication
   , apiVersion        :: Maybe Text
   , modules           :: Maybe Modules
   , enableLicensing   :: Maybe Bool
   , lifecycle         :: Maybe Lifecycle
   , links             :: Maybe [(Text, URI)]
   , scopes            :: Maybe [ProductScope]
   } deriving (Show, Generic)

data Key t a = Key t deriving (Show, Eq, Generic)

data PluginKey = PluginKey Text deriving (Show, Eq, Generic)

newtype Timeout = Timeout Second deriving (Show, Eq, Enum, Num, Ord, Real, Integral)

data Vendor = Vendor {vendorName :: Text, vendorUrl :: URI} deriving (Show, Eq, Generic)

data Authentication = Authentication {authType :: AuthType, publicKey :: Maybe Text} deriving (Show, Eq, Generic)

data AuthType = OAuth | Jwt | None deriving (Show, Eq, Generic)

data Modules = Modules JiraModules deriving (Show, Generic) -- TODO

data JiraModules = JiraModules
   { webPanels    :: [WebPanel]
   , generalPages :: [GeneralPage]
   , webhooks     :: [Webhook]
   } deriving (Show, Generic)

emptyJiraModules :: JiraModules
emptyJiraModules = JiraModules [] [] []


data WebPanel = WebPanel
   { wpKey        :: Text
   , wpName       :: Name WebPanel
   , wpUrl        :: Text
   , wpLocation   :: Text
   , wpConditions :: [Condition]
   } deriving (Show, Generic)

data GeneralPage = GeneralPage
   { generalPageUrl      :: Text
   , generalPageName     :: Name GeneralPage
   , generalPageKey      :: Text
   , generalPageLocation :: Maybe Text
   , generalPageIcon     :: Maybe IconDetails
   , generalPageWeight   :: Maybe Integer
   } deriving (Show, Generic)

data IconDetails = IconDetails
   { iconUrl    :: Text
   , iconWidth  :: Maybe Integer
   , iconHeight :: Maybe Integer
   } deriving (Show, Generic)

instance ToJSON PluginKey

data Name a = Name Text deriving (Show, Eq, Generic)

instance ToJSON (Name Plugin)
instance ToJSON (Name PluginKey)

instance ToJSON (Name WebPanel) where
   toJSON = nameToValue

instance ToJSON (Name GeneralPage) where
   toJSON = nameToValue

nameToValue :: Name a -> Value
nameToValue (Name name) = object [ "value" .= name ]

instance ToJSON Plugin where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "plugin"
      }

instance ToJSON GeneralPage where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "generalPage"
      }

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
   toJSON OAuth = "oauth"
   toJSON Jwt  = "jwt"
   toJSON None  = "none"

instance ToJSON Modules where
   toJSON = genericToJSON baseOptions

instance ToJSON JiraModules where
   toJSON = genericToJSON baseOptions

instance ToJSON WebPanel where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "wp"
      }

pluginDescriptor :: PluginKey -> URI -> Authentication -> Plugin
pluginDescriptor key' url' auth = Plugin
   { pluginKey = key'
   , pluginBaseUrl = url'
   , authentication = auth
   , pluginName = Nothing
   , pluginDescription = Nothing
   , vendor = Nothing
   , apiVersion = Nothing
   , modules = Nothing
   , enableLicensing = Nothing
   , lifecycle = Nothing
   , links = Nothing
   , scopes = Nothing
   }
