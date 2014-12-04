{-# LANGUAGE OverloadedStrings #-}
module ModulesTest (moduleTests) where

import           AssertionHelpers
import           Control.Applicative
import           Data.Aeson
import           Data.Connect.BaseTypes
import           Data.Connect.Conditions (JIRACondition (..),
                                          staticJiraCondition)
import           Data.Connect.Modules
import           Test.HUnit
import           ValueExtractors

moduleTests :: Test
moduleTests = TestList
    [ testWebPanelCorrectFormat
    , testGeneralPageCorrectFormat
    , testEmptyModulesAreShown
    ]

testWebPanelCorrectFormat :: Test
testWebPanelCorrectFormat = TestCase $ do
    let wp = WebPanel
              { wpKey = "web-panel-key"
              , wpName = Name "My web panel"
              , wpUrl = "/panel/my-panel?issue_key={issue.key}"
              , wpLocation = "atl.jira.view.issue.right.context"
              , wpConditions = [staticJiraCondition UserIsAdminJiraCondition]
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
    let gp = GeneralPage
              { generalPageKey = "general-page-key"
              , generalPageName = Name "General Page Name"
              , generalPageUrl = "/panel/page-panel-name?page_id={page.id}"
              , generalPageLocation = Just "some-confluence-location"
              , generalPageIcon = Just $ IconDetails "/path/to/icon" (Just 10) (Just 20)
              , generalPageWeight = Just 10000
              , generalPageConditions = [staticJiraCondition IsIssueReportedByCurrentUserJiraCondition]
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

testEmptyModulesAreShown :: Test
testEmptyModulesAreShown = TestCase $ do
    let modules = Modules emptyJIRAModules emptyConfluenceModules
    let jv = toJSON modules
    fieldIsPresent "webPanels" jv @? "Expected the field webPanels to be present"
    fieldIsPresent "generalPages" jv @? "Expected the field webPanels to be present"