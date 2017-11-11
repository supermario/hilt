module Hilt.Config where

import Text.Read                            (readMaybe)
import qualified System.Environment as E    (lookupEnv)
import Network.Wai                          (Middleware)
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Data.Maybe                           (fromMaybe)


data Environment =
    Development
  | Test
  | Staging
  | Production
  deriving (Eq, Show, Read)


lookupEnv :: Read a => String -> a -> IO a
lookupEnv name defaultVal = do
  param <- E.lookupEnv name
  pure $ case param of
    Nothing -> defaultVal
    Just a  -> fromMaybe defaultVal (readMaybe a)


lookupEnvString :: String -> String -> IO String
lookupEnvString name defaultVal = do
  param <- E.lookupEnv name
  pure $ fromMaybe defaultVal param


logger :: Environment -> Middleware
logger Test        = id
logger Development = logStdoutDev
logger Staging     = logStdoutDev
logger Production  = logStdout
