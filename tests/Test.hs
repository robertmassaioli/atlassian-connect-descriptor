module Test where

import           ConditionsTest
import qualified Distribution.TestSuite as TS
import qualified Test.HUnit             as HU
import           Util

hunitTests :: HU.Test
hunitTests = HU.TestList
    [ HU.TestLabel "Condition Tests" conditionsTests
    ]

tests :: IO [TS.Test]
tests = return [ TS.Test (hunitTestInstance hunitTests) ]
