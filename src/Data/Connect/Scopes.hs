{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Connect.Scopes
    ( ProductScope(..)
    ) where

import           Control.Monad (mzero)
import           Data.Aeson
import           GHC.Generics

-- | Scopes are an Atlassian Connect concept that declare how much access your addon requires to any give Cloud instance.
-- These scopes can be thought of as permissions are are well documented:
-- <https://developer.atlassian.com/static/connect/docs/scopes/scopes.html>
--
-- It is important to note that these scopes only give you restricted access to certain REST resources. You can not query
-- any REST url as you would with an Atlassian Server plugin. The restricted set of REST resources per application can be
-- found in the Atlassian Connect documentation.
data ProductScope
  = Read -- ^ The read scope means that you can pull data from the Cloud application.
  | Write -- ^ The write scope gives you the same access as a regular user of the Atlassian connect application.
  | Delete -- ^ The delete scope is required if you want to perform potentially destructive operations on data.
  | ProjectAdmin -- ^ A JIRA specific scope. Lets your add-on administer a project in JIRA.
  | SpaceAdmin -- ^ A Confluence specific scope. Lets your add-on administer a space in Confluence.
  | Admin -- ^ Gives your Atlassian Connect add-on administrative rights to the Cloud instance. (But NOT system
          -- administrator permission. Happily you cannot request that level of access.)
  | ActAsUser -- ^ Add-ons with this scope can access resources and perform actions in JIRA and Confluence on behalf of users.
  deriving (Show, Eq, Generic)

instance ToJSON ProductScope where
   toJSON Read         = "read"
   toJSON Write        = "write"
   toJSON Delete       = "delete"
   toJSON ProjectAdmin = "project_admin"
   toJSON SpaceAdmin   = "space_admin"
   toJSON Admin        = "admin"
   toJSON ActAsUser    = "act_as_user"

instance FromJSON ProductScope where
  parseJSON (String "read")             = return Read
  parseJSON (String "write")            = return Write
  parseJSON (String "delete")           = return Delete
  parseJSON (String "project_admin")    = return ProjectAdmin
  parseJSON (String "space_admin")      = return SpaceAdmin
  parseJSON (String "admin")            = return Admin
  parseJSON (String "act_as_user")      = return ActAsUser
  parseJSON _                           = mzero
