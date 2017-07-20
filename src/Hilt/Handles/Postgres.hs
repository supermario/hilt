{-# LANGUAGE Rank2Types #-}

module Hilt.Handles.Postgres
  (module Hilt.Handles.Postgres) where

import           Control.Monad.Logger         (NoLoggingT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Control.Monad.Reader         (ReaderT)
import           Database.Persist.Sql
import qualified Database.PostgreSQL.Simple as SQL


type DbInfo = [TableInfo]

data TableInfo = TableInfo
  { tableName :: String
  , fields :: [FieldInfo]
  } deriving (Show)

data FieldInfo = FieldInfo
  { fieldName :: String
  , fieldType :: String
  , fieldNullable :: Bool
  } deriving (Show)

-- @ISSUE Handle signature is specific for Persistent/PostgreSQL.Simple
data Handle = Handle
  { queryP :: forall a . ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
  , query_ :: forall a . (SQL.FromRow a) => SQL.Query -> IO [a]
  , query  :: forall a b . (SQL.FromRow a, SQL.ToRow b) => SQL.Query -> b -> IO [a]
  }

-- @TODO Usage sketches/ideas

  -- import Hilt.Database
  -- Hilt.Database.exec databaseH query
  -- let exec = Hilt.Database.exec databaseH
  -- results :: SomeType <- exec query


  -- import Hilt.Database as Database
  -- results :: SomeType <- Database.exec databaseH query


  -- import Hilt.Database (exec)
  -- results :: SomeType <- databaseH.exec query
