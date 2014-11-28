module ValueExtractors where

import           Data.Aeson
import qualified Data.HashMap.Strict as M
import           Data.Maybe          (maybe)
import qualified Data.Text           as T
import qualified Data.Vector as V

get :: T.Text -> Value -> IO Value
get key (Object o) = maybe notFound return $ M.lookup key o
    where
        notFound = fail $ "Could not find a value in object with key: " ++ T.unpack key
get key _ = fail $ "The value was not an object when looking for: " ++ T.unpack key

getArray :: Value -> IO [Value]
getArray (Array v) = return . V.toList $ v
getArray _ = fail "Value is not an array."

getString :: Value -> IO T.Text
getString (String s) = return s
getString _ = fail "Value is not a string value."

getBool :: Value -> IO Bool
getBool (Bool b) = return b
getBool _ = fail "Value is not a boolean."

isObject :: Value -> Bool
isObject (Object _) = True
isObject _ = False
