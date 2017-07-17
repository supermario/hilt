module Hilt.Logger (module Hilt.Logger, module Hilt.Handles.Logger) where

import Hilt.Handles.Logger
import Control.Monad.Managed (Managed, managed)

import Prelude hiding (log)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T

load :: Managed Handle
load = managed withHandle

withHandle :: (Handle -> IO a) -> IO a
withHandle f =
  f Handle
    { log = logImpl }

logImpl :: Priority -> T.Text -> IO ()
logImpl priority text = T.putStrLn $ "[" <> T.toLower (T.pack (show priority)) <> "] " <> text
