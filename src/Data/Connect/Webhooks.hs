{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Connect.Webhooks
   ( Webhook(..)
   , WebhookEvent(..)
   ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Connect.AesonHelpers
import           Data.Connect.OrphanInstances ()
import qualified Data.Text                    as T
import           GHC.Generics

-- | When users of the host application perform updates your Atlassian Connect add-on will not be alerted /unless/
-- it listens to the 'WebhookEvent's coming from that application. Webhooks are the way to close the issue recency loop
-- in the Atlassian products. It is important to note that Webhooks are 'best effort' and that there is no guarantee
-- that the webhook will make it to your Atlassian Connect application.
--
-- The Atlassian connect webhook documentation explains this in more detail:
-- <https://developer.atlassian.com/static/connect/docs/modules/jira/webhook.html>
data Webhook = Webhook
   { webhookEvent :: WebhookEvent -- ^ The event that you want your Atlassian Connect add-on to watch.
   , webhookUrl   :: T.Text -- ^ The relative URI that you wish to handle the webhook response.
   } deriving (Show, Generic)

instance ToJSON Webhook where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "webhook"
      }

-- | The webhook event that you wish to watch from your Atlassian Connect add-on.
data WebhookEvent
   = ConnectAddonDisabled
   | ConnectAddonEnabled
   | JiraWebhookPostFunction
   | JiraIssueCreated
   | JiraIssueDeleted
   | JiraIssueUpdated
   | JiraWorklogUpdated
   | JiraVersionCreated
   | JiraVersionDeleted
   | JiraVersionMerged
   | JiraVersionUpdated
   | JiraVersionMoved
   | JiraVersionReleased
   | JiraVersionUnreleased
   | JiraProjectCreated
   | JiraProjectUpdated
   | JiraProjectDeleted
   | JiraPluginEnabled
   | JiraPluginsUpgraded
   | JiraRemoteIssueLinkAggregateClearedEvent
   | JiraRemoteWorkflowPostFunction
   | JiraUserCreated
   | JiraUserDeleted
   | JiraUserUpdated
   | ConfluenceAttachmentCreated
   | ConfluenceAttachmentRemoved
   | ConfluenceAttachmentUpdated
   | ConfluenceAttachmentViewed
   | ConfluenceBlogCreated
   | ConfluenceBlogRemoved
   | ConfluenceBlogRestored
   | ConfluenceBlogTrashed
   | ConfluenceBlogUpdated
   | ConfluenceBlogViewed
   | ConfluenceCacheStatisticsChanged
   | ConfluenceCommentCreated
   | ConfluenceCommentRemoved
   | ConfluenceCommentUpdated
   | ConfluenceContentPermissionsUpdated
   | ConfluenceLabelAdded
   | ConfluenceLabelCreated
   | ConfluenceLabelDeleted
   | ConfluenceLabelRemoved
   | ConfluenceLogin
   | ConfluenceLoginFailed
   | ConfluenceLogout
   | ConfluencePageChildrenReordered
   | ConfluencePageCreated
   | ConfluencePageMoved
   | ConfluencePageRemoved
   | ConfluencePageRestored
   | ConfluencePageTrashed
   | ConfluencePageUpdated
   | ConfluencePageViewed
   | ConfluenceSearchPerformed
   | ConfluenceSpaceCreated
   | ConfluenceSpaceLogoUpdated
   | ConfluenceSpacePermissionsUpdated
   | ConfluenceSpaceRemoved
   | ConfluenceSpaceUpdated
   | ConfluenceStatusCleared
   | ConfluenceStatusCreated
   | ConfluenceStatusRemoved
   | ConfluenceUserCreated
   | ConfluenceUserDeactivated
   | ConfluenceUserFollowed
   | ConfluenceUserReactivated
   | ConfluenceUserRemoved
   | ConfluenceGroupCreated
   | ConfluenceGroupRemoved
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
   toJSON JiraVersionCreated = String "jira:version_created"
   toJSON JiraVersionDeleted = String "jira:version_deleted"
   toJSON JiraVersionMerged = String "jira:version_merged"
   toJSON JiraVersionUpdated = String "jira:version_updated"
   toJSON JiraVersionMoved = String "jira:version_moved"
   toJSON JiraVersionReleased = String "jira:version_released"
   toJSON JiraVersionUnreleased = String "jira:version_unreleased"
   toJSON JiraProjectCreated = String "jira:project_created"
   toJSON JiraProjectUpdated = String "jira:project_updated"
   toJSON JiraProjectDeleted = String "jira:project_deleted"
   toJSON JiraRemoteIssueLinkAggregateClearedEvent = String "remote_issue_link_aggregate_cleared_event"
   toJSON JiraRemoteWorkflowPostFunction = String "remote_workflow_post_function"
   toJSON JiraUserCreated = String "user_created"
   toJSON JiraUserDeleted = String "user_deleted"
   toJSON JiraUserUpdated = String "user_updated"
   toJSON ConfluenceAttachmentCreated = "attachment_created"
   toJSON ConfluenceAttachmentRemoved = "attachment_removed"
   toJSON ConfluenceAttachmentUpdated = "attachment_updated"
   toJSON ConfluenceAttachmentViewed = "attachment_viewed"
   toJSON ConfluenceBlogCreated = "blog_created"
   toJSON ConfluenceBlogRemoved = "blog_removed"
   toJSON ConfluenceBlogRestored = "blog_restored"
   toJSON ConfluenceBlogTrashed = "blog_trashed"
   toJSON ConfluenceBlogUpdated = "blog_updated"
   toJSON ConfluenceBlogViewed = "blog_viewed"
   toJSON ConfluenceCacheStatisticsChanged = "cache_statistics_changed"
   toJSON ConfluenceCommentCreated = "comment_created"
   toJSON ConfluenceCommentRemoved = "comment_removed"
   toJSON ConfluenceCommentUpdated = "comment_updated"
   toJSON ConfluenceContentPermissionsUpdated = "content_permissions_updated"
   toJSON ConfluenceLabelAdded = "label_added"
   toJSON ConfluenceLabelCreated = "label_created"
   toJSON ConfluenceLabelDeleted = "label_deleted"
   toJSON ConfluenceLabelRemoved = "label_removed"
   toJSON ConfluenceLogin = "login"
   toJSON ConfluenceLoginFailed = "login_failed"
   toJSON ConfluenceLogout = "logout"
   toJSON ConfluencePageChildrenReordered = "page_children_reordered"
   toJSON ConfluencePageCreated = "page_created"
   toJSON ConfluencePageMoved = "page_moved"
   toJSON ConfluencePageRemoved = "page_removed"
   toJSON ConfluencePageRestored = "page_restored"
   toJSON ConfluencePageTrashed = "page_trashed"
   toJSON ConfluencePageUpdated = "page_updated"
   toJSON ConfluencePageViewed = "page_viewed"
   toJSON ConfluenceSearchPerformed = "search_performed"
   toJSON ConfluenceSpaceCreated = "space_created"
   toJSON ConfluenceSpaceLogoUpdated = "space_logo_updated"
   toJSON ConfluenceSpacePermissionsUpdated = "space_permissions_updated"
   toJSON ConfluenceSpaceRemoved = "space_removed"
   toJSON ConfluenceSpaceUpdated = "space_updated"
   toJSON ConfluenceStatusCleared = "status_cleared"
   toJSON ConfluenceStatusCreated = "status_created"
   toJSON ConfluenceStatusRemoved = "status_removed"
   toJSON ConfluenceUserCreated = "user_created"
   toJSON ConfluenceUserDeactivated = "user_deactivated"
   toJSON ConfluenceUserFollowed = "user_followed"
   toJSON ConfluenceUserReactivated = "user_reactivated"
   toJSON ConfluenceUserRemoved = "user_removed"
   toJSON ConfluenceGroupCreated = "group_created"
   toJSON ConfluenceGroupRemoved = "group_removed"
   toJSON ServerUpgraded = String "server_upgraded"
