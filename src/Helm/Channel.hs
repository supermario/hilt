module Helm.Channel where

import qualified Data.Text as T

data Handle = Handle
  { read :: IO T.Text
  , write :: T.Text -> IO ()
  , worker :: (T.Text -> IO ()) -> IO()
  }
