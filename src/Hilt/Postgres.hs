{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Hilt.Postgres
  ( module Hilt.Postgres
  , module Hilt.Handles.Postgres
  ) where

import Text.Show.Pretty (ppShow)
import Hilt.Handles.Postgres
import Control.Monad.Managed (Managed, managed)

{-|

== Usage

Basic RDBMS style querying. Provides a couple of query functions while managing DB pool.

The service looks for a DATABASE_URL ENV var containing a postgresql URL on load, i.e;

"postgres://username:password@127.0.0.1:5432/databasename"

== Quick Example

@
-- TODO
@
-}
import Data.Monoid                         ((<>))
import Data.Maybe                          (fromMaybe)
import Control.Monad.Logger                (runNoLoggingT, runStdoutLoggingT)
import qualified Database.Persist.Sql as P (runSqlPersistMPool, ConnectionPool, insert)
import Database.Persist.Postgresql         (SqlBackend, ConnectionString, createPostgresqlPool)
import Database.Persist                    (Key, PersistEntityBackend, PersistEntity)

import qualified Database.PostgreSQL.Simple     as SQL
import qualified Database.PostgreSQL.Simple.URL as SQLU
import Database.PostgreSQL.Simple.SqlQQ         (sql)

import Network.URI (URIAuth, parseAbsoluteURI, uriScheme, uriAuthority, uriPath, uriRegName, uriPort, uriUserInfo, uriScheme)
import System.Environment (getEnv)

-- For readSqlChar
import Database.PostgreSQL.Simple.FromField  (returnError, ResultError (..), Field, Conversion)
import Text.Read                             (readMaybe)
import Data.ByteString                       (ByteString)
import Data.Text.Encoding                    (encodeUtf8)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Data.Text (Text)
import Data.Typeable

import qualified Hilt.Config as Config


load :: Managed Handle
load = managed withHandle


loadRaw :: IO Handle
loadRaw = do

  -- @ISSUE should this be contained in a config service...?
  env       <- Config.lookupEnv "ENV" Config.Development
  pool      <- makePool env
  rawConfig <- makePoolRaw
  conn      <- SQL.connect rawConfig

  -- @TODO how do implementations log? Should they demand a logger?
  -- putStrLn $ "RawConfig:" ++ rawConfig

  pure Handle
    { queryP  = flip P.runSqlPersistMPool pool
    , query_  = SQL.query_ conn
    , query   = qWrap conn
    , execute = SQL.execute conn
    , dbInfo  = dbInfoImpl conn
    }


withHandle :: (Handle -> IO a) -> IO a
withHandle f = loadRaw >>= f


-- Utilities

insert :: forall a . (PersistEntityBackend a ~ SqlBackend, PersistEntity a) => Handle -> a -> IO (Key a)
insert handle element = queryP handle $ P.insert element


-- Persistent Pool

makePool :: Config.Environment -> IO P.ConnectionPool
makePool e = do
  connStr <- lookupDatabaseUrl
  case e of
    Config.Test -> runNoLoggingT $ createPostgresqlPool connStr (envPoolSize e)
    _           -> runStdoutLoggingT $ createPostgresqlPool connStr (envPoolSize e)


-- @TODO this should be configurable
envPoolSize :: Config.Environment -> Int
envPoolSize Config.Development = 1
envPoolSize Config.Test        = 1
envPoolSize Config.Staging     = 1
envPoolSize Config.Production  = 8


-- PostgreSQL.Simple Pool

makePoolRaw :: IO SQL.ConnectInfo
makePoolRaw = do
  connStr <- Config.lookupEnvString "DATABASE_URL" ""
  pure $ fromMaybe defaultConnectInfo (SQLU.parseDatabaseUrl connStr)

-- @ISSUE read default from somewhere controllable by user?
defaultConnectInfo :: SQL.ConnectInfo
defaultConnectInfo = SQL.defaultConnectInfo { SQL.connectUser     = "postgres"
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


dbInfoImpl :: SQL.Connection -> IO DbInfo
dbInfoImpl conn = do
  tables <- singleColumnString conn dbInfoQuery
  mapM (getTableInfo conn) tables


pp :: forall a . Show a => a -> IO ()
pp a = putStrLn $ ppShow a


getTableInfo :: SQL.Connection -> String -> IO TableInfo
getTableInfo conn table = do
  tableInfosRaw :: [(String, String, Bool)] <- SQL.query conn tableInfoQuery [table]

  let fields = fmap (\(fieldName, fieldType, fieldNullable) -> FieldInfo {..}) tableInfosRaw

  pure $ TableInfo table fields


singleColumnString :: SQL.Connection -> SQL.Query -> IO [String]
singleColumnString conn q = do
  column :: [[String]] <- SQL.query_ conn q
  pure $ filter (/="") $ fmap
    ( \c -> case c of
      []    -> ""
      (x:_) -> x
    )
    column


dbInfoQuery :: SQL.Query
dbInfoQuery = [sql|
  SELECT tablename
  FROM pg_tables
  WHERE schemaname = ANY (current_schemas(false))
|]


tableInfoQuery :: SQL.Query
tableInfoQuery = [sql|
  SELECT a.attname, format_type(a.atttypid, a.atttypmod), not a.attnotnull
    FROM pg_attribute a
   WHERE a.attrelid = ?::regclass
     AND a.attnum > 0 AND NOT a.attisdropped
   ORDER BY a.attnum
|]


-- Ports of heroku and heroku-persistent code to make stack based installs easier
-- https://hackage.haskell.org/package/heroku-0.1.2.3/docs/src/Web-Heroku-Internal.html
-- https://hackage.haskell.org/package/heroku-persistent-0.2.0/docs/src/Web-Heroku-Persist-Postgresql.html

-- Fetch postgres formatted DATABASE_URL ENV var (ala Heroku) and return Persistant ConnectionString
lookupDatabaseUrl :: IO ConnectionString
lookupDatabaseUrl = do
  connStr <- formatParams <$> dbConnParams
  pure connStr


formatParams :: [(Text, Text)] -> ByteString
formatParams = encodeUtf8 . T.unwords . map toKeyValue


toKeyValue :: (Text, Text) -> Text
toKeyValue (k, v) = k <> "=" <> v


dbConnParams :: IO [(Text, Text)]
dbConnParams = dbConnParams' "DATABASE_URL" parseDatabaseUrl


parseDatabaseUrl :: String -> [(Text, Text)]
parseDatabaseUrl = parseDatabaseUrl' "postgres:"


-- | read the DATABASE_URL environment variable
-- and return an alist of connection parameters with the following keys:
-- user, password, host, port, dbname
--
-- warning: just calls error if it can't parse correctly
dbConnParams' :: String -> (String -> [(Text, Text)]) -> IO [(Text, Text)]
dbConnParams' envVar parse = fmap parse (getEnv envVar)


parseDatabaseUrl' :: String -> String -> [(Text, Text)]
parseDatabaseUrl' scheme durl =
  let muri         = parseAbsoluteURI durl
      (auth, path) = case muri of
        Nothing  -> error "couldn't parse absolute uri"
        Just uri -> if uriScheme uri /= scheme
          then schemeError uri
          else case uriAuthority uri of
            Nothing -> invalid
            Just a  -> (a, uriPath uri)
      (user, password) = userAndPassword auth
  in  [ ( T.pack "user"
        , user
        )
           -- tail not safe, but should be there on Heroku
      , (T.pack "password", T.tail password)
      , (T.pack "host"    , T.pack $ uriRegName auth)
      , ( T.pack "port"
        , T.pack $ removeColon $ uriPort auth
        )
         -- tail not safe but path should always be there
      , (T.pack "dbname", T.pack $ Prelude.tail path)
      ]
 where
  removeColon (':':port) = port
  removeColon port       = port

  -- init is not safe, but should be there on Heroku
  userAndPassword :: URIAuth -> (Text, Text)
  userAndPassword = T.breakOn (T.pack ":") . T.pack . Prelude.init . uriUserInfo

  schemeError uri = error $ "was expecting a postgres scheme, not: " ++ uriScheme uri ++ "\n" ++ show uri
  -- should be an error
  invalid = error "could not parse heroku DATABASE_URL"
