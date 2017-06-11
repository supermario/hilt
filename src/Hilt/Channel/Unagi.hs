module Hilt.Channel.Unagi (load, withHandle) where

import Control.Monad                (forever)
import SlaveThread as ST

import Prelude hiding (read)
import Control.Concurrent.Chan.Unagi

import qualified Hilt.Channel as Channel

import Control.Monad.Managed (Managed, managed)

load :: Managed Channel.Handle
load = managed withHandle

withHandle :: (Channel.Handle -> IO a) -> IO a
withHandle f = do
  (write,read) <- newChan

  f Channel.Handle
    { Channel.read = readChan read
    , Channel.write = writeChan write
    , Channel.worker = workerImpl read
    }

workerImpl :: OutChan t -> (t -> IO a) -> IO ()
workerImpl chan handler = do
  -- Should we really be throwing away ThreadId ?
  _ <- ST.fork $ forever $ do
    text <- readChan chan
    handler text
  return ()
