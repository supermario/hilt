module Hilt.Handles.Logger
  (module Hilt.Handles.Logger) where

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

debug, info, warning, error :: Handle -> T.Text -> IO ()
debug   = (`log` Debug)
info    = (`log` Info)
warning = (`log` Warning)
error   = (`log` Error)
