{-# LANGUAGE OverloadedStrings #-}

-- | A logger implementation that logs all messages to a 'System.IO.Handle'.
module Helm.Logger.StdOut (withHandle) where

import qualified Helm.Logger as Logger
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T

withHandle :: (Logger.Handle -> IO a) -> IO a
withHandle f =
  f Logger.Handle
    { Logger.log = logImpl }

logImpl :: Logger.Priority -> T.Text -> IO ()
logImpl priority text = T.putStrLn $ "[" <> T.pack (show priority) <> "] " <> text
