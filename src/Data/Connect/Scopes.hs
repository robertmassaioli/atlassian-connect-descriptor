{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Connect.Scopes
    ( ProductScope(..)
    ) where

import           Control.Monad (mzero)
import           Data.Aeson
import           GHC.Generics

data ProductScope
  = Read
  | Write
  | Delete
  | ProjectAdmin   -- This is a JIRA only Scope (TODO can we serve the correct scope set to the correct plugins?)
  | SpaceAdmin    -- This is a Confluence only Scope (TODO can we serve the correct scope set to the correct plugins?)
  | Admin
  deriving (Show, Eq, Generic)

instance ToJSON ProductScope where
   toJSON Read         = "read"
   toJSON Write        = "write"
   toJSON Delete       = "delete"
   toJSON ProjectAdmin = "project_admin"
   toJSON SpaceAdmin   = "space_admin"
   toJSON Admin        = "admin"

instance FromJSON ProductScope where
  parseJSON (String "read")             = return Read
  parseJSON (String "write")            = return Write
  parseJSON (String "delete")           = return Delete
  parseJSON (String "project_admin")    = return ProjectAdmin
  parseJSON (String "space_admin")      = return SpaceAdmin
  parseJSON (String "admin")            = return Admin
  parseJSON _                           = mzero
