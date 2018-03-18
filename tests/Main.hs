module Main where

import           ConditionsTest
import           DescriptorTest
import           ModulesTest
import qualified Test.HUnit             as HU
import           System.Exit (exitWith, ExitCode(..))

hunitTests :: HU.Test
hunitTests = HU.TestList
    [ HU.TestLabel "Condition Tests" conditionsTests
    , HU.TestLabel "Module Tests" moduleTests
    , HU.TestLabel "Descriptor Tests" descriptorTests
    ]

main :: IO ()
main = do
    counts <- HU.runTestTT hunitTests
    let mistakes = HU.errors counts + HU.failures counts
    exitWith (if mistakes > 0 then ExitFailure mistakes else ExitSuccess)