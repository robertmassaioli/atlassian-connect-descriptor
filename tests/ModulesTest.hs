{-# LANGUAGE OverloadedStrings #-}
module ModulesTest (moduleTests) where

import Control.Applicative
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

isEqualTo :: (Eq a, Show a) => IO a -> a -> Assertion
isEqualTo actual expected = do
    value <- actual
    value @?= expected

testGeneralPageCorrectFormat :: Test
testGeneralPageCorrectFormat = TestCase $ do
    return ()
