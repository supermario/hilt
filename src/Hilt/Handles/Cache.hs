module Hilt.Handles.Cache
  (module Hilt.Handles.Cache) where

import Data.Text (Text)

data Handle = Handle
  { insert :: Text -> Text -> IO ()
  , lookup :: Text -> IO (Maybe Text)
  , delete :: Text -> IO ()
  , keys   :: IO [Text]
  , size   :: IO Int
  }
