{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Connect.Modules
   ( Modules(..)
   , JiraModules(..)
   , emptyJiraModules
   , ConfluenceModules(..)
   , emptyConfluenceModules
   , WebPanel(..)
   , GeneralPage(..)
   ) where

import           Control.Monad             (mzero)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Connect.AesonHelpers
import           Data.Connect.BaseTypes
import           Data.Connect.Conditions
import           Data.Connect.Webhooks
import qualified Data.HashMap.Strict       as HM
import qualified Data.Text                 as T
import           GHC.Generics

-- TODO all of these need to be compressed together into the same "modules" block
data Modules = Modules
   { jiraModules       :: JiraModules
   , confluenceModules :: ConfluenceModules
   } deriving (Show, Generic)

instance ToJSON Modules where
   toJSON modules = case (jm, cm) of
      (Object jiraObject, Object confluenceObject) -> Object $ HM.union jiraObject confluenceObject
      _ -> Null
      where
         jm = toJSON . jiraModules $ modules
         cm = toJSON . confluenceModules $ modules

-- TODO use Endo Modules to add modules for multiple different products to the modules list
data JiraModules = JiraModules
   { jiraWebPanels    :: [WebPanel]
   , jiraWeneralPages :: [GeneralPage]
   , jiraWebhooks     :: [Webhook]
   } deriving (Show, Generic)

instance ToJSON JiraModules where
   toJSON = genericToJSON baseOptions

data ConfluenceModules = ConfluenceModules
   { confluenceWebPanels :: [WebPanel]
   } deriving (Show, Generic)

instance ToJSON ConfluenceModules where
   toJSON = genericToJSON baseOptions

emptyJiraModules :: JiraModules
emptyJiraModules = JiraModules [] [] []

emptyConfluenceModules :: ConfluenceModules
emptyConfluenceModules = ConfluenceModules []

data WebPanel = WebPanel
   { wpKey        :: T.Text
   , wpName       :: Name WebPanel
   , wpUrl        :: T.Text
   , wpLocation   :: T.Text
   , wpConditions :: [Condition]
   } deriving (Show, Generic)

instance ToJSON WebPanel where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "wp"
      }

data GeneralPage = GeneralPage
   { generalPageUrl      :: T.Text
   , generalPageName     :: Name GeneralPage
   , generalPageKey      :: T.Text
   , generalPageLocation :: Maybe T.Text
   , generalPageIcon     :: Maybe IconDetails
   , generalPageWeight   :: Maybe Integer
   } deriving (Show, Generic)

instance ToJSON GeneralPage where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "generalPage"
      }

instance ToJSON (Name WebPanel) where
   toJSON = nameToValue

instance ToJSON (Name GeneralPage) where
   toJSON = nameToValue

nameToValue :: Name a -> Value
nameToValue (Name name) = object [ T.pack "value" .= name ]
