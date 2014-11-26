{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Data.Connect.BaseTypes where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Connect.AesonHelpers
import           Data.Connect.OrphanInstances
import           Data.Text
import qualified Data.Time.Units              as DTU
import           GHC.Generics
import qualified Network.URI                  as NU

data Key t a = Key t deriving (Show, Eq, Generic)

data PluginKey = PluginKey Text deriving (Show, Eq, Generic)

newtype Timeout = Timeout DTU.Second deriving (Show, Eq, Enum, Num, Ord, Real, Integral)

data Vendor = Vendor {vendorName :: Text, vendorUrl :: NU.URI} deriving (Show, Eq, Generic)

data Authentication = Authentication {authType :: AuthType, publicKey :: Maybe Text} deriving (Show, Eq, Generic)

data AuthType = OAuth | Jwt | None deriving (Show, Eq, Generic)

data IconDetails = IconDetails
   { iconUrl    :: Text
   , iconWidth  :: Maybe Integer
   , iconHeight :: Maybe Integer
   } deriving (Show, Generic)

instance ToJSON PluginKey

data Name a = Name Text deriving (Show, Eq, Generic)

instance ToJSON (Name PluginKey)

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
