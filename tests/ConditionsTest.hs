module ConditionsTest (conditionsTests) where

import Data.Connect.Conditions
import Test.HUnit

conditionsTests :: Test
conditionsTests = TestList
    [ invertConditionSimple
    , invertConditionAndType
    , invertConditionOrType
    , invertConditionNested
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

-- Confluence Conditions come out correctly
-- Remote conditions come out correctly
