module AssertionHelpers where

import           Test.HUnit

isEqualTo :: (Eq a, Show a) => IO a -> a -> Assertion
isEqualTo actual expected = do
    value <- actual
    value @?= expected