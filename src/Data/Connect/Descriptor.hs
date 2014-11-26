{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- TODO the descriptor needs to be extracted into more modules and perhaps its own library

-- TODO be more selective about what we export from here
module Data.Connect.Descriptor
   ( Plugin(..)
   , pluginDescriptor
   , Key(..)
   , PluginKey(..)
   , Timeout(..)
   , Vendor(..)
   , Authentication(..)
   , AuthType(..)
   ) where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Connect.AesonHelpers
import           Data.Connect.BaseTypes
import           Data.Connect.Conditions
import           Data.Connect.Lifecycle
import           Data.Connect.Modules
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

instance ToJSON (Name Plugin)

instance ToJSON Plugin where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "plugin"
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
