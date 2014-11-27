module Test where

import qualified Distribution.TestSuite as TS
import qualified Test.HUnit             as HU
import           Util

test1 :: HU.Test
test1 = HU.TestCase (HU.assertEqual "one equals three" 1 3)

hunitTests :: HU.Test
hunitTests = HU.TestList [HU.TestLabel "Test 1" test1]

tests :: IO [TS.Test]
tests = return [ TS.Test (hunitTestInstance hunitTests) ]
