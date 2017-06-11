module Hilt.PostgresSimple (readSqlChar) where

import Database.PostgreSQL.Simple.FromField       (returnError, ResultError (..), Field, Conversion)
import Text.Read                                  (readMaybe)
import qualified Data.ByteString.Char8 as B
import Data.Typeable

readSqlChar :: (Typeable a, Read a) => Field -> Maybe B.ByteString -> Conversion a
readSqlChar f bs = case bs of
  Nothing -> returnError UnexpectedNull f ""
  Just x  -> case readMaybe $ B.unpack x of
    Just a  -> pure a
    Nothing -> returnError ConversionFailed f ""


-- Use pg_typeof(derp) to inspect type of record return

-- @TODO - this would be cool? Just work for anything that can be Read?
--         it does however result in "Overlapping instances for FromField Int"
--         so maybe we need to add a different typeclass to custom fields? i.e
--         instance PgSimpleReadable a => FromField a where
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- instance PgCharReadable a => FromField a where
--   fromField f bs = case bs of
--     Nothing -> returnError UnexpectedNull f ""
--     Just x  -> case (readMaybe $ B.unpack x) of
--       Just a  -> pure a
--       Nothing -> returnError ConversionFailed f ""

-- @TODO add type check for char column?
-- if typeOid f /= $(inlineTypoid TI.char) then returnError Incompatible f ""
