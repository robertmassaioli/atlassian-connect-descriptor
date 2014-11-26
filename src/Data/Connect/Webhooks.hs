{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Connect.Webhooks
   ( Webhook(..)
   , WebhookEvent(..)
   ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Connect.AesonHelpers
import qualified Data.Text                 as T
import           GHC.Generics

data Webhook = Webhook
   { webhookEvent :: WebhookEvent
   , webhookUrl   :: T.Text
   } deriving (Show, Generic)

instance ToJSON Webhook where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "webhook"
      }

data WebhookEvent
   = ConnectAddonDisabled
   | ConnectAddonEnabled
   | JiraWebhookPostFunction
   | JiraIssueCreated
   | JiraIssueDeleted
   | JiraIssueUpdated
   | JiraWorklogUpdated
   | JiraPluginEnabled
   | JiraPluginsUpgraded
   | JiraRemoteIssueLinkAggregateClearedEvent
   | JiraRemoteWorkflowPostFunction
   | ServerUpgraded
   deriving (Show)

instance ToJSON WebhookEvent where
   toJSON ConnectAddonDisabled = String "connect_addon_disabled"
   toJSON ConnectAddonEnabled = String "connect_addon_enabled"
   toJSON JiraWebhookPostFunction = String "jira-webhook-post-function"
   toJSON JiraIssueCreated = String "jira:issue_created"
   toJSON JiraIssueDeleted = String "jira:issue_deleted"
   toJSON JiraIssueUpdated = String "jira:issue_updated"
   toJSON JiraWorklogUpdated = String "jira:worklog_updated"
   toJSON JiraPluginEnabled = String "plugin_enabled"
   toJSON JiraPluginsUpgraded = String "plugins_upgraded"
   toJSON JiraRemoteIssueLinkAggregateClearedEvent = String "remote_issue_link_aggregate_cleared_event"
   toJSON JiraRemoteWorkflowPostFunction = String "remote_workflow_post_function"
   toJSON ServerUpgraded = String "server_upgraded"

-- TODO add the confluence webhook events