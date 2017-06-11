module Hilt.Logger where

import Prelude hiding (log)
import qualified Data.Text as T

data Priority
  = Debug    -- ^ Debug messages
  | Info     -- ^ Notable information that requires no immediate action.
  | Warning  -- ^ Something is probably wrong, and we should investigate.
  | Error    -- ^ Something is wrong and immediate action is required.
  deriving (Eq, Ord, Show)

newtype Handle = Handle
  { log :: Priority -> T.Text -> IO () }

logDebug, logInfo, logWarning, logError :: Handle -> T.Text -> IO ()
logDebug   = (`log` Debug)
logInfo    = (`log` Info)
logWarning = (`log` Warning)
logError   = (`log` Error)
