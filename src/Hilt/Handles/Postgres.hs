{-# LANGUAGE Rank2Types #-}

module Hilt.Handles.Postgres
  (module Hilt.Handles.Postgres) where

import           Control.Monad.Logger         (NoLoggingT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Control.Monad.Reader         (ReaderT)
import           Database.Persist.Sql
import qualified Database.PostgreSQL.Simple as SQL

-- @ISSUE Handle signature is specific for Persistent/PostgreSQL.Simple
data Handle = Handle
  { exec   :: forall a . ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
  , execR  :: forall a . (SQL.FromRow a) => SQL.Query -> IO [a]
  , execRP :: forall a b . (SQL.FromRow a, SQL.ToRow b) => SQL.Query -> b -> IO [a]
  -- @TODO fix naming to execRaw / execRawParam
  -- @TODO add seperate implementations for execRawParam and execRawParams
  -- with the former solving the (x,1) issue and the latter handling the tuples
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
