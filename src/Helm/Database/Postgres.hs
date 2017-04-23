module Helm.Database.Postgres (load, withHandle) where

{-|

== Usage

Basic RDBMS style querying. Provides a couple of query functions while managing DB pool.

The service looks for a DATABASE_URL ENV var containing a postgresql URL on load.

== Quick Example

@
-- TODO
@
-}
import Data.Maybe                  (fromMaybe)
import Control.Monad.Logger        (runNoLoggingT, runStdoutLoggingT)
import Database.Persist.Sql        (runSqlPersistMPool, ConnectionPool)
import Database.Persist.Postgresql (ConnectionString, createPostgresqlPool, pgConnStr)
import Database.Persist.Sqlite     (SqliteConf(..), createSqlitePool)

import qualified Database.PostgreSQL.Simple     as SQL
import qualified Database.PostgreSQL.Simple.URL as SQLU

import qualified Web.Heroku.Persist.Postgresql  as Heroku

import qualified Helm.Database as Database
import qualified Helm.Config as Config

import Control.Monad.Managed (Managed, managed)

load :: Managed Database.Handle
load = managed withHandle

withHandle :: (Database.Handle -> IO a) -> IO a
withHandle f = do

  -- @ISSUE this should be contained in a config service
  env       <- Config.lookupEnv "ENV" Config.Development

  pool      <- makePool env
  rawConfig <- makePoolRaw

  -- @TODO how do implementations log? Should they demand a logger?
  -- putStrLn $ "RawConfig:" ++ rawConfig

  f Database.Handle
    { Database.exec = flip runSqlPersistMPool pool
    , Database.execR = runDbRaw rawConfig
    , Database.execRP = runDbRawP rawConfig
    }


runDbRaw :: (SQL.FromRow a) => SQL.ConnectInfo -> SQL.Query -> IO [a]
runDbRaw creds query = do
  conn <- SQL.connect creds
  SQL.query_ conn query


runDbRawP :: (SQL.FromRow a, SQL.ToRow b) => SQL.ConnectInfo -> SQL.Query -> b -> IO [a]
runDbRawP creds query p = do
  conn <- SQL.connect creds
  SQL.query conn query p



-- Persistent Pool

makePool :: Config.Environment -> IO ConnectionPool
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
    , SQL.connectDatabase = "helm_development"
    }
