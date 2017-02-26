{-# LANGUAGE Rank2Types #-}

{-|
Database service
-}
module Helm.Database where

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
  }

-- @TODO Usage sketches/ideas

  -- import Helm.Database
  -- Helm.Database.exec databaseH query
  -- let exec = Helm.Database.exec databaseH
  -- results :: SomeType <- exec query


  -- import Helm.Database as Database
  -- results :: SomeType <- Database.exec databaseH query


  -- import Helm.Database (exec)
  -- results :: SomeType <- databaseH.exec query
