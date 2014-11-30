module Test where

import           ConditionsTest
import           DescriptorTest
import qualified Distribution.TestSuite as TS
import           ModulesTest
import qualified Test.HUnit             as HU
import           Util

hunitTests :: HU.Test
hunitTests = HU.TestList
    [ HU.TestLabel "Condition Tests" conditionsTests
    , HU.TestLabel "Module Tests" moduleTests
    , HU.TestLabel "Descriptor Tests" descriptorTests
    ]

tests :: IO [TS.Test]
tests = return [ TS.Test (hunitTestInstance hunitTests) ]
