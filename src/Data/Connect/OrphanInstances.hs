module Data.Connect.OrphanInstances where

import           Control.Monad (mzero)
import           Data.Aeson
import qualified Data.Text     as T
import qualified Network.URI   as NU

instance FromJSON NU.URI where
   parseJSON (String uriString) = maybe mzero return (NU.parseURI . T.unpack $ uriString)
   parseJSON _ = mzero

instance ToJSON NU.URI where
   toJSON = String . T.pack . show