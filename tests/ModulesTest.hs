{-# LANGUAGE OverloadedStrings #-}
module ModulesTest (moduleTests) where

import           AssertionHelpers
import           Control.Applicative
import           Data.Aeson
import           Data.Connect.Descriptor
import qualified Data.HashMap.Strict     as HM
import           Test.HUnit
import           ValueExtractors

moduleTests :: Test
moduleTests = TestList
    [ testWebPanelCorrectFormat
    , testGeneralPageCorrectFormat
    , testEmptyModulesAreNotShown
    , testIssueGlanceCorrectFormat
    ]

testIssueGlanceCorrectFormat :: Test
testIssueGlanceCorrectFormat = TestCase $ do
    let jig = JIRAIssueGlance
                { jigKey = "jira-issue-glance-module"
                , jigName = simpleText "My glance"
                , jigContent = JIRAIssueGlanceContentLabel (simpleText "my label")
                , jigIcon = IconDetails
                    { iconUrl = "my_icon.svg"
                    , iconWidth = Just 24
                    , iconHeight = Just 25
                    }
                , jigTarget = JIRAIssueGlanceTargetWebPanel ("/panel_url")
                , jigConditions = []
                }
    let jv = toJSON jig
    isObject jv @? "Expected the issue glance to be an object"
    (getString =<< get "key" jv) `isEqualTo` "jira-issue-glance-module"
    (getString =<< get "value" =<< get "name" jv) `isEqualTo` "My glance"
    (getString =<< get "type" =<< get "target" jv) `isEqualTo` "web_panel"
    (getString =<< get "url" =<< get "target" jv) `isEqualTo` "/panel_url"
    (getString =<< get "type" =<< get "content" jv) `isEqualTo` "label"
    (getString =<< get "value" =<< get "label" =<< get "content" jv) `isEqualTo` "my label"
    (get "url" =<< get "icon" jv) `isEqualTo` "my_icon.svg"
    (getNumber =<< get "width" =<< get "icon" jv) `isEqualTo` 24
    (getNumber =<< get "height" =<< get "icon" jv) `isEqualTo` 25

testWebPanelCorrectFormat :: Test
testWebPanelCorrectFormat = TestCase $ do
    let wp = WebPanel
              { wpKey = "web-panel-key"
              , wpName = simpleText "My web panel"
              , wpUrl = "/panel/my-panel?issue_key={issue.key}"
              , wpLocation = "atl.jira.view.issue.right.context"
              , wpConditions = [staticJiraCondition UserIsAdminJiraCondition]
              , wpTooltip = Just . simpleText $ "A tooltip for the web panel..."
              , wpWeight = Just 1234
              , wpLayout = Just $ WebPanelLayout (Pixels 321) (Percentage 80)
              , wpParams = HM.fromList [("one", "1"), ("two", "2")]
              }
    let jv = toJSON wp
    isObject jv @? "Expected the web panel to be an object"
    (getString =<< get "key" jv) `isEqualTo` "web-panel-key"
    (getString =<< get "value" =<< get "name" jv) `isEqualTo` "My web panel"
    (getString =<< get "url" jv) `isEqualTo` "/panel/my-panel?issue_key={issue.key}"
    (getString =<< get "location" jv) `isEqualTo` "atl.jira.view.issue.right.context"
    (isArray <$> get "conditions" jv) @? "Expected the conditions to be an array"

testGeneralPageCorrectFormat :: Test
testGeneralPageCorrectFormat = TestCase $ do
    let gp = JIRAPage
              { jiraPageKey = "general-page-key"
              , jiraPageName = simpleText "General Page Name"
              , jiraPageUrl = "/panel/page-panel-name?page_id={page.id}"
              , jiraPageLocation = Just "some-confluence-location"
              , jiraPageIcon = Just $ IconDetails "/path/to/icon" (Just 10) (Just 20)
              , jiraPageWeight = Just 10000
              , jiraPageConditions = [staticJiraCondition IsIssueReportedByCurrentUserJiraCondition]
              , jiraPageParams = HM.fromList [("gpOne", "gp1"), ("gpTwo", "gp2")]
              }
    let jv = toJSON gp
    isObject jv @? "Expected the web panel to be an object"
    get "key" jv `isEqualTo` "general-page-key"
    (get "value" =<< get "name" jv) `isEqualTo` "General Page Name"
    get "url" jv `isEqualTo` "/panel/page-panel-name?page_id={page.id}"
    get "location" jv `isEqualTo` "some-confluence-location"
    (isObject <$> get "icon" jv) @? "Expected an icon to be present"
    (get "url" =<< get "icon" jv) `isEqualTo` "/path/to/icon"
    (getNumber =<< get "width" =<< get "icon" jv) `isEqualTo` 10
    (getNumber =<< get "height" =<< get "icon" jv) `isEqualTo` 20
    (getNumber =<< get "weight" jv) `isEqualTo` 10000
    (isArray <$> get "conditions" jv) @? "Expected the conditions to be present."

testEmptyModulesAreNotShown :: Test
testEmptyModulesAreNotShown = TestCase $ do
    let modules = Modules emptyJIRAModules emptyConfluenceModules
    let jv = toJSON modules
    fieldIsNotPresent "webPanels" jv @? "Expected the field webPanels to be present"
    fieldIsNotPresent "generalPages" jv @? "Expected the field webPanels to be present"
