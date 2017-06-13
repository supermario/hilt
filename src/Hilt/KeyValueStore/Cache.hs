module Hilt.KeyValueStore.Cache (load, withHandle) where

import Prelude hiding (lookup)

import Data.Cache
import Data.Monoid           ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T


import qualified Hilt.KeyValueStore as KeyValueStore

import Control.Monad.Managed (Managed, managed)

load :: Managed KeyValueStore.Handle
load = managed withHandle

withHandle :: (KeyValueStore.Handle -> IO a) -> IO a
withHandle f = do
  c <- newCache Nothing :: IO (Cache T.Text T.Text)

  f KeyValueStore.Handle
    { KeyValueStore.insert = insertImpl c
    , KeyValueStore.lookup = lookup c
    , KeyValueStore.delete = delete c
    , KeyValueStore.keys   = keys c
    , KeyValueStore.size   = size c
    }

insertImpl :: Cache T.Text T.Text -> T.Text -> T.Text -> IO ()
insertImpl cache key value = do
  T.putStrLn ("[Debug] KeyValueStore:insert:" <> T.pack (show value))
  insert cache key value
