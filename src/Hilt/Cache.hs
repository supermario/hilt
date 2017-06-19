module Hilt.Cache (module Hilt.Cache, module Hilt.Handles.Cache) where

import Hilt.Handles.Cache
import Control.Monad.Managed (Managed, managed)

import Prelude hiding (lookup)

import qualified Data.Cache as Cache
import qualified Data.Text as T

load :: Managed Handle
load = managed withHandle

withHandle :: (Handle -> IO a) -> IO a
withHandle f = do
  c <- Cache.newCache Nothing :: IO (Cache.Cache T.Text T.Text)

  f Handle
    { insert = Cache.insert c
    , lookup = Cache.lookup c
    , delete = Cache.delete c
    , keys   = Cache.keys c
    , size   = Cache.size c
    }
