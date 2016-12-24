-- | A logger implementation that logs all messages to a 'System.IO.Handle'.
module Helm.Storage.TVar (load, withHandle) where

import qualified Helm.Logger as Logger
import qualified Helm.Storage as Storage
import qualified Data.Text as T
import           Data.Monoid ((<>))

import Control.Monad.Managed (Managed, managed)

load :: Logger.Handle -> Managed Storage.Handle
load loggerH = managed $ withHandle loggerH

withHandle :: Logger.Handle -> (Storage.Handle -> IO a) -> IO a
withHandle loggerH f =
  f Storage.Handle
    { Storage.write = writeImpl loggerH }

writeImpl :: Logger.Handle -> T.Text -> IO ()
writeImpl loggerH text =
  Logger.logDebug loggerH $ "Got a hit from writeImpl:" <> text
