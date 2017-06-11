module Hilt.Config where

import Text.Read                            (readMaybe)
import qualified System.Environment as E    (lookupEnv)
import Network.Wai                          (Middleware)
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)

data Environment =
    Development
  | Test
  | Staging
  | Production
  deriving (Eq, Show, Read)

lookupEnv :: Read a => String -> a -> IO a
lookupEnv name defaultVal = do
  param <- E.lookupEnv name
  return $ case param of
    Nothing -> defaultVal
    Just a  -> case readMaybe a of
      Nothing -> defaultVal
      Just b  -> b

lookupEnvString :: String -> String -> IO String
lookupEnvString name defaultVal = do
  param <- E.lookupEnv name
  return $ case param of
    Nothing -> defaultVal
    Just a  -> a

logger :: Environment -> Middleware
logger Test        = id
logger Development = logStdoutDev
logger Staging     = logStdoutDev
logger Production  = logStdout
