module Hilt.Storage where

import qualified Data.Text as T

newtype Handle = Handle
  { write :: T.Text -> IO () }
