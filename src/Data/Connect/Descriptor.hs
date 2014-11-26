{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- TODO the descriptor needs to be extracted into more modules and perhaps its own library

-- TODO be more selective about what we export from here
module Data.Connect.Descriptor where

import           Data.Connect.AesonHelpers
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
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

data Name a = Name Text deriving (Show, Eq, Generic)

data Key t a = Key t deriving (Show, Eq, Generic)

data PluginKey = PluginKey Text deriving (Show, Eq, Generic)

newtype Timeout = Timeout Second deriving (Show, Eq, Enum, Num, Ord, Real, Integral)

data Vendor = Vendor {vendorName :: Text, vendorUrl :: URI} deriving (Show, Eq, Generic)

data Authentication = Authentication {authType :: AuthType, publicKey :: Maybe Text} deriving (Show, Eq, Generic)

data AuthType = OAuth | Jwt | None deriving (Show, Eq, Generic)

data Modules = Modules JiraModules deriving (Show, Generic) -- TODO

data ProductScope
  = Read
  | Write
  | Delete
  | ProjectAdmin   -- This is a JIRA only Scope (TODO can we serve the correct scope set to the correct plugins?)
  | SpaceAdmin    -- This is a Confluence only Scope (TODO can we serve the correct scope set to the correct plugins?)
  | Admin
  deriving (Show, Eq, Generic)

data JiraModules = JiraModules
   { webPanels    :: [WebPanel]
   , generalPages :: [GeneralPage]
   , webhooks     :: [Webhook]
   } deriving (Show, Generic)

emptyJiraModules :: JiraModules
emptyJiraModules = JiraModules [] [] []

data Condition = SingleCondition
   { conditionSource   :: ConditionSource
   , conditionInverted :: Bool
   -- , conditionParams    :: [(String, String)] -- TODO impliment properly but not required yet
   }
   | CompositeCondition
   { subConditions :: [Condition]
   , conditionType :: ConditionType
   }
   deriving (Show)

staticJiraCondition :: JIRACondition -> Condition
staticJiraCondition c = SingleCondition { conditionSource = StaticJIRACondition c, conditionInverted = False }

staticConfluenceCondition :: ConfluenceCondition -> Condition
staticConfluenceCondition c = SingleCondition { conditionSource = StaticConfluenceCondition c, conditionInverted = False }

remoteCondition :: String -> Condition
remoteCondition conditionLocation = SingleCondition { conditionSource = RemoteCondition conditionLocation, conditionInverted = False }

invertCondition :: Condition -> Condition
invertCondition c = c { conditionInverted = not . conditionInverted $ c }

instance ToJSON Condition where
   toJSON sc@(SingleCondition {}) = object
      [ "condition" .= conditionSource sc
      , "invert" .= conditionInverted sc
      ]
   toJSON cc@(CompositeCondition {}) = object [ "conditions" .= subConditions cc, "type" .= conditionType cc]

data ConditionType = AndCondition | OrCondition
   deriving (Eq, Show)

instance ToJSON ConditionType where
   toJSON AndCondition = String "AND"
   toJSON OrCondition  = String "OR"

data ConditionSource
   = StaticJIRACondition        JIRACondition
   | StaticConfluenceCondition  ConfluenceCondition
   | RemoteCondition
      { remoteConditionPath :: String
      }
   deriving (Show, Eq)

instance ToJSON ConditionSource where
   toJSON (StaticJIRACondition x) = toJSON x
   toJSON (StaticConfluenceCondition x) = toJSON x
   toJSON (RemoteCondition x) = toJSON x

-- The JIRA Conditions have been taken from:
-- https://developer.atlassian.com/static/connect/docs/modules/fragment/single-condition.html
-- as of the following date: Tue 23 Sep 2014 08:45:49 EST
-- Please update the date above whenever you update these conditions.
data JIRACondition
   = CanAttachFileToIssueJiraCondition
   | CanManageAttachmentsJiraCondition
   | FeatureFlagJiraCondition
   | HasIssuePermissionJiraCondition
   | HasProjectPermissionJiraCondition
   | HasSelectedProjectPermissionJiraCondition
   | HasSubTasksAvaliableJiraCondition
   | HasVotedForIssueJiraCondition
   | IsAdminModeJiraCondition
   | IsIssueAssignedToCurrentUserJiraCondition
   | IsIssueEditableJiraCondition
   | IsIssueReportedByCurrentUserJiraCondition
   | IsIssueUnresolvedJiraCondition
   | IsSubTaskJiraCondition
   | IsWatchingIssueJiraCondition
   | LinkingEnabledJiraCondition
   | SubTasksEnabledJiraCondition
   | TimeTrackingEnabledJiraCondition
   | UserHasIssueHistoryJiraCondition
   | UserIsAdminJiraCondition
   | UserIsLoggedInJiraCondition
   | UserIsProjectAdminJiraCondition
   | UserIsSysadminJiraCondition
   | UserIsTheLoggedInUserJiraCondition
   | VotingEnabledJiraCondition
   | WatchingEnabledJiraCondition
   deriving (Eq, Show, Generic)

instance ToJSON JIRACondition where
   toJSON = toJSON . dropSuffixAndSnakeCase "JiraCondition" . show

data ConfluenceCondition
   = ActiveThemeConfluenceCondition
   | CanEditSpaceStylesConfluenceCondition
   | CanSignupConfluenceCondition
   | ContentHasAnyPermissionsSetConfluenceCondition
   | CreateContentConfluenceCondition
   | EmailAddressPublicConfluenceCondition
   | FavouritePageConfluenceCondition
   | FavouriteSpaceConfluenceCondition
   | FeatureFlagConfluenceCondition
   | FollowingTargetUserConfluenceCondition
   | HasAttachmentConfluenceCondition
   | HasBlogPostConfluenceCondition
   | HasPageConfluenceCondition
   | HasSpaceConfluenceCondition
   | HasTemplateConfluenceCondition
   | LatestVersionConfluenceCondition
   | NotPersonalSpaceConfluenceCondition
   | PrintableVersionConfluenceCondition
   | ShowingPageAttachmentsConfluenceCondition
   | SpaceFunctionPermissionConfluenceCondition
   | SpaceSidebarConfluenceCondition
   | TargetUserCanSetStatusConfluenceCondition
   | TargetUserHasPersonalBlogConfluenceCondition
   | TargetUserHasPersonalSpaceConfluenceCondition
   | ThreadedCommentsConfluenceCondition
   | TinyUrlSupportedConfluenceCondition
   | UserCanCreatePersonalSpaceConfluenceCondition
   | UserCanUpdateUserStatusConfluenceCondition
   | UserCanUseConfluenceConfluenceCondition
   | UserFavouritingTargetUserPersonalSpaceConfluenceCondition
   | UserHasPersonalBlogConfluenceCondition
   | UserHasPersonalSpaceConfluenceCondition
   | UserIsAdminConfluenceCondition
   | UserIsConfluenceAdministratorConfluenceCondition
   | UserIsLoggedInConfluenceCondition
   | UserIsSysadminConfluenceCondition
   | UserLoggedInEditableConfluenceCondition
   | UserWatchingPageConfluenceCondition
   | UserWatchingSpaceConfluenceCondition
   | UserWatchingSpaceForContentTypeConfluenceCondition
   | ViewingContentConfluenceCondition
   | ViewingOwnProfileConfluenceCondition
   deriving (Eq, Show, Generic)

instance ToJSON ConfluenceCondition where
   toJSON = toJSON . dropSuffixAndSnakeCase "ConfluenceCondition" . show

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

data Webhook = Webhook
   { webhookEvent :: WebhookEvent
   , webhookUrl   :: Text
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

data Lifecycle = Lifecycle
   { installed   :: Maybe URI
   , uninstalled :: Maybe URI
   , enabled     :: Maybe URI
   , disabled    :: Maybe URI
   } deriving (Show, Generic) -- TODO

instance ToJSON PluginKey

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

instance ToJSON URI where
   toJSON = String . pack . show

instance ToJSON Lifecycle where
   toJSON = genericToJSON baseOptions

instance ToJSON Modules where
   toJSON = genericToJSON baseOptions

instance ToJSON ProductScope where
   toJSON Read        = "read"
   toJSON Write        = "write"
   toJSON Delete       = "delete"
   toJSON ProjectAdmin   = "project_admin"
   toJSON SpaceAdmin    = "space_admin"
   toJSON Admin        = "admin"

instance FromJSON ProductScope where
  parseJSON (String "read")        = return Read
  parseJSON (String "write")       = return Write
  parseJSON (String "delete")      = return Delete
  parseJSON (String "project_admin")  = return ProjectAdmin
  parseJSON (String "space_admin")   = return SpaceAdmin
  parseJSON (String "admin")       = return Admin
  parseJSON _                 = mzero

instance ToJSON JiraModules where
   toJSON = genericToJSON baseOptions

instance ToJSON WebPanel where
   toJSON = genericToJSON baseOptions
      { fieldLabelModifier = stripFieldNamePrefix "wp"
      }

instance FromJSON URI where
   parseJSON (String uriString) = maybe mzero return (parseURI $ unpack uriString)
   parseJSON _ = mzero

defaultLifecycle :: Lifecycle
defaultLifecycle = Lifecycle
   { installed = parseRelativeReference "/installed"
   , uninstalled = parseRelativeReference "/uninstalled"
   , enabled = parseRelativeReference "/enabled"
   , disabled = parseRelativeReference "/disabled"
   }

pluginDescriptor :: PluginKey -> URI -> Authentication -> Plugin
pluginDescriptor key' url' auth = Plugin
   { pluginName = Nothing
   , pluginDescription = Nothing
   , pluginKey = key'
   , pluginBaseUrl = url'
   , vendor = Nothing
   , authentication = auth
   , apiVersion = Nothing
   , modules = Nothing
   , enableLicensing = Nothing
   , lifecycle = Nothing
   , links = Nothing
   , scopes = Nothing
   }
