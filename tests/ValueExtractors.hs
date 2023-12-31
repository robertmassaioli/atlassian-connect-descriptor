module ValueExtractors where

import           Control.Applicative
import           Data.Aeson
import qualified Data.HashMap.Strict as M
import           Data.Maybe          (isJust)
import qualified Data.Scientific     as S
import qualified Data.Text           as T
import qualified Data.Vector         as V

get :: T.Text -> Value -> IO Value
get key (Object o) = maybe notFound return $ M.lookup key o
    where
        notFound = fail $ "Could not find a value in object with key: " ++ T.unpack key
get key _ = fail $ "The value was not an object when looking for: " ++ T.unpack key

fieldIsPresent :: T.Text -> Value -> IO Bool
fieldIsPresent key (Object o) = return . isJust $ M.lookup key o
fieldIsPresent key _ = fail $ "The value was not an object when looking for: " ++ T.unpack key

fieldIsNotPresent :: T.Text -> Value -> IO Bool
fieldIsNotPresent key value = not <$> fieldIsPresent key value

getArray :: Value -> IO [Value]
getArray (Array v) = return . V.toList $ v
getArray v = fail $ "Value is not an array. It is a: " ++ valueAsString v

getString :: Value -> IO T.Text
getString (String s) = return s
getString v = fail $ "Value is not a string value. It is a: " ++ valueAsString v

getBool :: Value -> IO Bool
getBool (Bool b) = return b
getBool v = fail $ "Value is not a boolean. It is a: " ++ valueAsString v

isObject :: Value -> Bool
isObject (Object _) = True
isObject _ = False

isArray :: Value -> Bool
isArray (Array _) = True
isArray _ = False

getNumber :: Value -> IO S.Scientific
getNumber (Number n) = return n
getNumber v = fail $ "Value is not a number. It is a: " ++ valueAsString v

valueAsString :: Value -> String
valueAsString (Object _) = "Object"
valueAsString (Array _) = "Array"
valueAsString (Bool _) = "Bool"
valueAsString (String _) = "String"
valueAsString (Number _) = "Number"
valueAsString Null = "Null"
