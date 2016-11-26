{-# LANGUAGE OverloadedStrings #-}

module Helm.Channel.Stm (withHandle) where

import qualified Data.Text as T

import Control.Concurrent           (forkIO)
import Control.Monad                (forever)
import Control.Concurrent.STM       (STM, atomically)
import Control.Concurrent.STM.TChan (TChan, newTChan, readTChan, writeTChan)

import qualified Helm.Channel as Channel

withHandle :: (Channel.Handle -> IO a) -> IO a
withHandle f = do
  chan <- atomically (do newTChan :: STM (TChan a))

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
  _ <- forkIO $ forever $ do
    text <- atomically $ readTChan chan
    handler text
  return ()
