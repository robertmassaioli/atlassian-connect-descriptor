module Util
    ( hunitTestInstance
    ) where

import qualified Distribution.TestSuite as TS
import qualified Test.HUnit             as HU

runHUnitTests :: HU.Test -> IO TS.Progress
runHUnitTests tests = do
   (HU.Counts cases tried errors failures) <- HU.runTestTT tests
   return $ if errors > 0
      then TS.Finished $ TS.Error "There were errors in the HUnit tests"
      else if failures > 0
         then TS.Finished $ TS.Fail "There were failures in the HUnit tests"
         else TS.Finished TS.Pass

hunitTestInstance :: HU.Test -> TS.TestInstance
hunitTestInstance hTests = TS.TestInstance
    { TS.run = runHUnitTests hTests
    , TS.name = "HUnit Tests"
    , TS.tags = ["hunit"]
    , TS.options = []
    , TS.setOption = \_ _ -> Right (hunitTestInstance hTests)
    }

