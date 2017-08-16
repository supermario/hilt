{-# LANGUAGE Rank2Types #-}

{-|
Module      : Hilt
Description : A set of managed IO services for Haskell
Copyright   : (c) Mario Rogic, 2016
License     : GPL-3
Maintainer  : hello@mario.net.au
Stability   : experimental
Portability : POSIX

== Motivation

Hilt is a batteries-included implementation of the [service pattern](https://www.schoolofhaskell.com/user/meiersi/the-service-pattern).

It eschews type class and mtl approaches (ala ['Scrap your type classes'](http://www.haskellforall.com/2012/05/scrap-your-type-classes.html)) to experiment with a more implicit value level approach.

It is intended to be used at the base level of your application, providing some structure for your IO-generating business logic.

== Service Types

Hilt currently provides interfaces for the following types of services:

* Logger: for Debug, Info, Warning and Error level logging
* Channel: typed read/write channels with workers
* Database: basic RDBMS style querying
* SocketServer: websocket server

== Implementations

Hilt provides the following implementations:

* Logger.StdOut: prints logged items to stdout
* Channel.Stm: A software-transactional-memory implementation of channels
* Hilt.Database.Postgres: provides a couple of query functions while managing DB pool
* Hilt.SocketServer

    * Hilt.SocketServer.Echo: echoes back client responses without any handling
    * Hilt.SocketServer.Hooked: takes extra function i.e. `onJoined name totalClients :: IO ()` that will be run for every client join

They can be used directly, or simply as reference code to pull out and create your own services as needed.

For example you may want a storage-backed channel that survives program restarts, so you might create your own, or implement Hilt.Channel.DB which takes a Hilt.Database handler for persistence.

== Quick Example

@
module Main where

import           Data.Monoid              ((<>))
import           Hilt
import qualified Hilt
import qualified Hilt.Channel             as Channel
import qualified Hilt.Channel.Stm         as Channel.Stm
import qualified Hilt.Logger              as Logger
import qualified Hilt.Logger.StdOut       as Logger.StdOut

main :: IO ()
main = Hilt.manage $ do
  loggerH    <- Logger.StdOut.load
  broadcastH <- Channel.Stm.load

  Hilt.program $ do
    Logger.logDebug loggerH "Handlers initiating..."

    -- Write our business logic here directly, i.e. logging all broadcast messages
    Channel.worker broadcastH (\text -> Logger.logDebug loggerH $ "broadcast-worker:" <> text)

    -- Or pass services off to some other function that implements the relevant business logic for our app
    run loggerH broadcastH
@
-}
module Hilt
  ( -- * Running
    manage
  , once
  , program
  , manageTest)
  where

import Control.Concurrent    (threadDelay)
import Control.Monad         (forever)
import Control.Monad.Managed (Managed, MonadIO, liftIO, runManaged)
import System.IO (hSetBuffering, hSetEncoding, BufferMode(..), stdout, utf8)


-- | Wrapper for runManaged that than runs forever. See example above for usage.
manage :: forall a. Managed a -> IO ()
manage things =
  runManaged $ do
    liftIO stdoutSetup
    _ <- things
    -- Wait until the the process is killed
    forever $ liftIO $ threadDelay 100000


-- | Wrapper for runManaged that than runs once. See example above for usage.
once :: forall a. Managed a -> IO ()
once things =
  runManaged $ do
    liftIO stdoutSetup
    _ <- things
    return ()


-- | Force LineBuffering for consistent output behavior
stdoutSetup :: IO ()
stdoutSetup = do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8


-- | TBC, for testing.
manageTest :: Managed () -> IO ()
manageTest = runManaged


-- | Utility function re-exporting liftIO to avoid Control.Monad.Managed dependency in Hilt client code.
program :: MonadIO m => IO a -> m a
program = liftIO
