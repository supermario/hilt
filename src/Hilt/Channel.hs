module Hilt.Channel (module Hilt.Channel, module Hilt.Handles.Channel) where

import Hilt.Handles.Channel
import Control.Monad.Managed (Managed, managed)

import Control.Monad                (forever)
import SlaveThread as ST
import Prelude hiding (read)
import Control.Concurrent.Chan.Unagi

-- Implementation using the Unagi-chan package
load :: Managed Handle
load = managed withHandle

withHandle :: (Handle -> IO a) -> IO a
withHandle f = do
  (w,r) <- newChan

  f Handle
    { read = readChan r
    , write = writeChan w
    , worker = workerImpl r
    }

workerImpl :: OutChan t -> (t -> IO a) -> IO ()
workerImpl chan handler = do
  -- Should we really be throwing away ThreadId ?
  _ <- ST.fork $ forever $ do
    text <- readChan chan
    handler text
  return ()
