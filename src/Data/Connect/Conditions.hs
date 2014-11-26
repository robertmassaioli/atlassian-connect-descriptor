{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Connect.Conditions where

import           Data.Aeson
import           Data.Connect.AesonHelpers
import           GHC.Generics

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
