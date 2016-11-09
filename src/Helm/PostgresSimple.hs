module Helm.PostgresSimple (readSqlChar) where

import Database.PostgreSQL.Simple.FromField       (typeOid, returnError, ResultError (..), Field, Conversion)
import Database.PostgreSQL.Simple.TypeInfo.Static (typoid)
import Text.Read                                  (readMaybe)
import qualified Data.ByteString.Char8 as B
import Data.Typeable


readSqlChar :: (Typeable a, Read a) => Field -> Maybe B.ByteString -> Conversion a
readSqlChar f bs = case bs of
  Nothing -> returnError UnexpectedNull f ""
  Just x  -> case readMaybe $ B.unpack x of
    Just a  -> pure a
    Nothing -> returnError ConversionFailed f ""
