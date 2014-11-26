module Data.Connect.AesonHelpers
   ( baseOptions
   , stripFieldNamePrefix
   , dropSuffixAndSnakeCase
   ) where

import qualified Cases            as CS
import           Data.Aeson.Types
import qualified Data.Char        as C
import qualified Data.List        as L
import           Data.Maybe       (fromMaybe)
import qualified Data.Text        as T

baseOptions :: Options
baseOptions = defaultOptions
   { omitNothingFields = True
   }

stripFieldNamePrefix :: String -> String -> String
stripFieldNamePrefix pre = lowerFirst . try (L.stripPrefix pre)

dropSuffixAndSnakeCase :: String -> String -> String
dropSuffixAndSnakeCase suffix = T.unpack . CS.process CS.lower CS.snake . T.pack . dropSuffix suffix

lowerFirst :: String -> String
lowerFirst (x : xs) = C.toLower x : xs
lowerFirst [] = []

dropSuffix :: String -> String -> String
dropSuffix suffix = reverse . try (L.stripPrefix . reverse $ suffix) . reverse

try :: (a -> Maybe a) -> a -> a
try f v = fromMaybe v (f v)
