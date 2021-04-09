{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Connect.Conditions
    ( Condition(..)
    , ConditionType(..)
    , ConditionSource(..)
    , JIRACondition(..)
    , ConfluenceCondition(..)
    , staticJiraCondition
    , staticConfluenceCondition
    , remoteCondition
    , invertCondition
    ) where

import           Data.Aeson
import           Data.Connect.AesonHelpers
import           Data.Connect.OrphanInstances ()
import           GHC.Generics
import qualified Data.Text           as T
import qualified Data.HashMap.Strict as HM

-- | A 'Condition' can be placed on an Atlassian Connect Module to cause it to display or not based on the result it
-- returns. For example, you can choose not to show a WebPanel if the user viewing the page is not logged in. Conditions
-- are very useful in curating when your modules will appear to your users.
--
-- The Atlassian Connect documentation describes conditions fully:
-- <https://developer.atlassian.com/static/connect/docs/concepts/conditions.html>
data Condition
   -- | A single condition based on a source.
   = SingleCondition
   { conditionSource   :: ConditionSource -- ^ The source of this condition.
   , conditionInverted :: Bool -- ^ If you should invert the condition. For example, only show if user is NOT logged in.
   , conditionParams :: HM.HashMap String String -- ^ Extra parameters to pass with the condition to give it context.
   }
   -- | A condition that is the composition of one or more conditions. The 'ConditionType' decides the way in which the
   -- conditions are composed
   | CompositeCondition
   { subConditions :: [Condition] -- ^ The conditions that will be merged together.
   , conditionType :: ConditionType -- ^ The way in which the conditions will be merged together.
   }
   deriving (Show)

-- | Turn a standard JIRA Condition into a regular 'Condition'.
staticJiraCondition :: JIRACondition -> Condition
staticJiraCondition c = SingleCondition { conditionSource = StaticJIRACondition c, conditionInverted = False, conditionParams = HM.empty }

-- | Turn a standard Confluence Condition into a regular 'Condition'.
staticConfluenceCondition :: ConfluenceCondition -> Condition
staticConfluenceCondition c = SingleCondition { conditionSource = StaticConfluenceCondition c, conditionInverted = False, conditionParams = HM.empty }

-- | Given a URI that defines a remote condition convert it into a regular 'Condition'.
remoteCondition :: String -> Condition
remoteCondition conditionLocation = SingleCondition { conditionSource = RemoteCondition conditionLocation, conditionInverted = False, conditionParams = HM.empty }

-- | Invert the given condition.
invertCondition :: Condition -> Condition
invertCondition c@(SingleCondition {}) = c { conditionInverted = not . conditionInverted $ c }
-- One application of DeMorgans law for composite conditions.
invertCondition (CompositeCondition cs ct) = CompositeCondition (fmap invertCondition cs) (invertConditionType ct)

invertConditionType :: ConditionType -> ConditionType
invertConditionType AndCondition = OrCondition
invertConditionType OrCondition = AndCondition

instance ToJSON Condition where
   toJSON sc@(SingleCondition {}) = object
      [ "condition" .= conditionSource sc
      , "invert" .= conditionInverted sc
      , "params" .= conditionParams sc
      ]
   toJSON cc@(CompositeCondition {}) = object [ compositionConditionKey cc .= subConditions cc]

compositionConditionKey :: Condition -> T.Text
compositionConditionKey (CompositeCondition _ AndCondition) = "and"
compositionConditionKey (CompositeCondition _ OrCondition) = "or"
compositionConditionKey _ = error "This method should not have been passed a non-composite condition"

-- | Composite Conditions can be joined together to behave as a single condition. The way that you can join them together
-- is decided by the condition type.
data ConditionType
   = AndCondition -- ^ The boolean intersection of the conditions.
   | OrCondition -- ^ The boolean union of the conditions.
   deriving (Eq, Show)

instance ToJSON ConditionType where
   toJSON AndCondition = String "AND"
   toJSON OrCondition  = String "OR"

-- | Conditions can be specified by the Host application or by the Atlassian Connect add-on itself. This means that the
-- source of the condition needs to be specified and that is what you can use this data type to do.
data ConditionSource
   -- | A static JIRA condition.
   = StaticJIRACondition        JIRACondition
   -- | A static Confluence condition.
   | StaticConfluenceCondition  ConfluenceCondition
   -- | A remote condition defined by your Atlassian Connect application.
   | RemoteCondition
      { remoteConditionPath :: String -- ^ The relative URI that you should hit in your Atlassian Connect application to
                                      -- get the condition result. This URI, when hit, should return a JSON response in
                                      -- the format:
                                      --
                                      -- > { "shouldDisplay": <true|false> }
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

-- | The conditions that have been provided by JIRA. Please see the single condition documentation for more details:
-- <https://developer.atlassian.com/static/connect/docs/modules/fragment/single-condition.html>
data JIRACondition
   = CanAttachFileToIssueJiraCondition
   | CanManageAttachmentsJiraCondition
   | EntityPropertyEqualToJiraCondition
   | FeatureFlagJiraCondition
   | HasIssuePermissionJiraCondition
   | HasProjectPermissionJiraCondition -- ^ Returns true if there is a selected project and the user has project admin to it.
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

-- | The conditions that have been provided by Confluence. Please see the single condition documentation for more details:
-- <https://developer.atlassian.com/static/connect/docs/modules/fragment/single-condition.html>
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
