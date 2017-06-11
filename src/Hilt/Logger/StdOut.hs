-- | A logger implementation that logs all messages to a 'System.IO.Handle'.
module Hilt.Logger.StdOut (load, withHandle) where

import qualified Hilt.Logger as Logger
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad.Managed (Managed, managed)

load :: Managed Logger.Handle
load = managed withHandle

withHandle :: (Logger.Handle -> IO a) -> IO a
withHandle f =
  f Logger.Handle
    { Logger.log = logImpl }

logImpl :: Logger.Priority -> T.Text -> IO ()
logImpl priority text = T.putStrLn $ "[" <> T.pack (show priority) <> "] " <> text
