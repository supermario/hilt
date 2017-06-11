module Hilt.Channel.Stm (load, withHandle) where

import Control.Monad                (forever)
import Control.Concurrent.STM       (STM, atomically)
import Control.Concurrent.STM.TChan (TChan, newTChan, readTChan, writeTChan)
import SlaveThread as ST

import qualified Hilt.Channel as Channel

import Control.Monad.Managed (Managed, managed)

load :: Managed Channel.Handle
load = managed withHandle

withHandle :: (Channel.Handle -> IO a) -> IO a
withHandle f = do
  chan <- atomically (newTChan :: STM (TChan a))

  f Channel.Handle
    { Channel.read = readImpl chan
    , Channel.write = writeImpl chan
    , Channel.worker = workerImpl chan
    }

readImpl :: TChan a -> IO a
readImpl chan =
  atomically $ readTChan chan

writeImpl :: TChan a -> a -> IO ()
writeImpl chan text =
  atomically $ writeTChan chan text

workerImpl :: TChan a -> (a -> IO ()) -> IO ()
workerImpl chan handler = do
  -- Should we really be throwing away ThreadId ?
  _ <- ST.fork $ forever $ do
    text <- atomically $ readTChan chan
    handler text
  return ()
