module ConditionsTest (conditionsTests) where

import Data.Connect.Conditions
import Test.HUnit

conditionsTests :: Test
conditionsTests = TestList
    [ invertConditionSimple
    ]

-- Inverting conditions works correctly
invertConditionSimple :: Test
invertConditionSimple = TestCase $ do
    let originalCondition = staticJiraCondition UserIsAdminJiraCondition
    (not . conditionInverted $ originalCondition) @? "Expected the condition to not be inverted on creation."
    let invertedCondition = invertCondition originalCondition
    conditionInverted invertedCondition @? "Expected the condition to be inverted."


-- JIRA Conditions come out correctly
-- Confluence Conditions come out correctly
-- Remote conditions come out correctly
