{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Hilt.Postgres
  ( module Hilt.Postgres
  , module Hilt.Handles.Postgres
  ) where

import Hilt.Handles.Postgres
import Control.Monad.Managed (Managed, managed)

{-|

== Usage

Basic RDBMS style querying. Provides a couple of query functions while managing DB pool.

The service looks for a DATABASE_URL ENV var containing a postgresql URL on load.

== Quick Example

@
-- TODO
@
-}
import Data.Maybe                          (fromMaybe)
import Control.Monad.Logger                (runNoLoggingT, runStdoutLoggingT)
import qualified Database.Persist.Sql as P (runSqlPersistMPool, ConnectionPool, insert)
import Database.Persist.Postgresql         (SqlBackend, ConnectionString, createPostgresqlPool, pgConnStr)
import Database.Persist.Sqlite             (SqliteConf(..), createSqlitePool)
import Database.Persist                    (Key, PersistEntityBackend, PersistEntity)

import qualified Database.PostgreSQL.Simple     as SQL
import qualified Database.PostgreSQL.Simple.URL as SQLU
import qualified Web.Heroku.Persist.Postgresql  as Heroku

-- For readSqlChar
import Database.PostgreSQL.Simple.FromField       (returnError, ResultError (..), Field, Conversion)
import qualified Data.ByteString.Char8 as B
import Text.Read                                  (readMaybe)
import Data.Typeable

import qualified Hilt.Config as Config


load :: Managed Handle
load = managed withHandle


withHandle :: (Handle -> IO a) -> IO a
withHandle f = do

  -- @ISSUE should this be contained in a config service...?
  env       <- Config.lookupEnv "ENV" Config.Development
  pool      <- makePool env
  rawConfig <- makePoolRaw
  conn      <- SQL.connect rawConfig

  -- @TODO how do implementations log? Should they demand a logger?
  -- putStrLn $ "RawConfig:" ++ rawConfig

  f Handle
    { queryP = flip P.runSqlPersistMPool pool
    , query_ = SQL.query_ conn
    , query = SQL.query conn
    }


-- Utilities

insert :: forall a . (PersistEntityBackend a ~ SqlBackend, PersistEntity a)
  => Handle -> a -> IO (Key a)
insert handle element = queryP handle $ P.insert element


-- Persistent Pool

makePool :: Config.Environment -> IO P.ConnectionPool
makePool Config.Test = runNoLoggingT $ createSqlitePool (sqlDatabase $ sqliteConf Config.Test) (envPoolSize Config.Test)
makePool e = do
  -- Development / Staging / Production envs use Postgres and DATABASE_URL
  connStr <- lookupDatabaseUrl
  runStdoutLoggingT $ createPostgresqlPool connStr (envPoolSize e)


-- For staging/prod
-- Fetch postgres formatted DATABASE_URL ENV var (ala Heroku) and return Persistant ConnectionString
lookupDatabaseUrl :: IO ConnectionString
lookupDatabaseUrl = pgConnStr <$> Heroku.postgresConf 1 -- The 1 is dropped as we only pull out pgConnStr from the ADT


envPoolSize :: Config.Environment -> Int
envPoolSize Config.Development = 1
envPoolSize Config.Test        = 1
envPoolSize Config.Staging     = 1
envPoolSize Config.Production  = 8


sqliteConf :: Config.Environment -> SqliteConf
sqliteConf Config.Test = SqliteConf ":memory:" 1
sqliteConf Config.Development = SqliteConf "./tmp/db-dev.sqlite" 1
sqliteConf _ = undefined


-- PostgreSQL.Simple Pool

makePoolRaw :: IO SQL.ConnectInfo
makePoolRaw = do
  connStr <- Config.lookupEnvString "DATABASE_URL" ""
  return $ fromMaybe defaultConnectInfo (SQLU.parseDatabaseUrl connStr)

-- @ISSUE read default from someone controllable by user?
defaultConnectInfo :: SQL.ConnectInfo
defaultConnectInfo = SQL.defaultConnectInfo
    { SQL.connectUser = "postgres"
    , SQL.connectPassword = ""
    , SQL.connectDatabase = "hilt_development"
    }


-- Utilities

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
