{-# LANGUAGE OverloadedStrings #-}
module ConditionsTest (conditionsTests) where

import           AssertionHelpers
import           Control.Monad
import           Data.Aeson
import           Data.Connect.Conditions
import qualified Data.Text               as T
import           Test.HUnit
import           ValueExtractors
import qualified Data.HashMap.Strict as HM

conditionsTests :: Test
conditionsTests = TestList
    [ invertConditionSimple
    , invertConditionAndType
    , invertConditionOrType
    , invertConditionNested
    , jiraConditionJsonCorrect
    , confluenceConditionJsonCorrect
    , remoteConditionJsonCorrect
    , compositeAndConditionConnect
    , conditionParamsTest
    ]

-- Inverting conditions works correctly
invertConditionSimple :: Test
invertConditionSimple = TestCase $ do
    let originalCondition = staticJiraCondition UserIsAdminJiraCondition
    (not . conditionInverted $ originalCondition) @? "Expected the condition to not be inverted on creation."
    let invertedCondition = invertCondition originalCondition
    conditionInverted invertedCondition @? "Expected the condition to be inverted."

threeChildConditions :: [Condition]
threeChildConditions =
    [ invertCondition $ staticJiraCondition UserHasIssueHistoryJiraCondition
    , staticJiraCondition IsWatchingIssueJiraCondition
    , staticJiraCondition HasVotedForIssueJiraCondition
    ]

invertConditionAndType :: Test
invertConditionAndType = TestCase $ do
    let invertedCondition = invertCondition $ CompositeCondition threeChildConditions AndCondition
    OrCondition @=? conditionType invertedCondition
    [False, True, True] @=? (fmap conditionInverted . subConditions $ invertedCondition)

invertConditionOrType :: Test
invertConditionOrType = TestCase $ do
    let invertedCondition = invertCondition $ CompositeCondition threeChildConditions OrCondition
    AndCondition @=? conditionType invertedCondition
    [False, True, True] @=? (fmap conditionInverted . subConditions $ invertedCondition)

invertConditionNested :: Test
invertConditionNested = TestCase $ do
    let child1 = invertCondition $ CompositeCondition threeChildConditions OrCondition
    let child2 = CompositeCondition threeChildConditions OrCondition
    let child3 = CompositeCondition threeChildConditions AndCondition
    let invertedCondition = invertCondition $ CompositeCondition [child1, child2, child3] AndCondition
    OrCondition @=? conditionType invertedCondition
    [OrCondition, AndCondition, OrCondition] @=? (fmap conditionType . subConditions $ invertedCondition)
    [[True, False, False], [False, True, True], [False, True, True]] @=? (fmap (fmap conditionInverted . subConditions) . subConditions $ invertedCondition)

-- JIRA Conditions come out correctly
jiraConditionJsonCorrect :: Test
jiraConditionJsonCorrect = TestCase $ do
    let condition = staticJiraCondition UserIsAdminJiraCondition
    let jsonValue = toJSON condition
    isObject jsonValue @? "Expect the result to be an object"
    (getString =<< get "condition" jsonValue) `isEqualTo` "user_is_admin"
    (return . not =<< getBool =<< get "invert" jsonValue) @? "Expected the condition to not be inverted"

-- Confluence Conditions come out correctly
confluenceConditionJsonCorrect :: Test
confluenceConditionJsonCorrect = TestCase $ do
    let condition = invertCondition $ staticConfluenceCondition HasAttachmentConfluenceCondition
    let jsonValue = toJSON condition
    isObject jsonValue @? "Expect the result to be an object"
    (getString =<< get "condition" jsonValue) `isEqualTo` "has_attachment"
    (getBool =<< get "invert" jsonValue) @? "Expect the condition to be inverted"

-- Remote conditions come out correctly
remoteConditionJsonCorrect :: Test
remoteConditionJsonCorrect = TestCase $ do
    let remoteUrl = "/rest/condition/is-awesome"
    let condition = invertCondition $ remoteCondition remoteUrl
    let jsonValue = toJSON condition
    isObject jsonValue @? "Expect the remote condition to come out as an object."
    (getString =<< get "condition" jsonValue) `isEqualTo` T.pack remoteUrl
    (getBool =<< get "invert" jsonValue) @? "Expect the condition to be inverted"

-- CompositeCondition's are rendered correctly
compositeAndConditionConnect :: Test
compositeAndConditionConnect = TestCase $ do
    let condition = CompositeCondition threeChildConditions AndCondition
    let jsonValue = toJSON condition
    isObject jsonValue @? "Expect the conditions in an object."
    conditions <- getArray =<< get "conditions" jsonValue
    sequence (fmap (getString <=< get "condition") conditions) `isEqualTo` ["user_has_issue_history","is_watching_issue","has_voted_for_issue"]
    get "type" jsonValue `isEqualTo` "AND"

conditionParamsTest :: Test
conditionParamsTest = TestCase $ do
    let condition = staticJiraCondition UserIsAdminJiraCondition
    let conditionWithParams = condition { conditionParams = HM.fromList [("param-one", "value-one"), ("param-two", "value-two")] }
    let jv = toJSON conditionWithParams
    (getString =<< get "param-one" =<< get "params" jv) `isEqualTo` "value-one"
    (getString =<< get "param-two" =<< get "params" jv) `isEqualTo` "value-two"