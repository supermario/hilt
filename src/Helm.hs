{-# LANGUAGE Rank2Types #-}

{-|
Module      : Helm
Description : A set of managed IO services for Haskell
Copyright   : (c) Mario Rogic, 2016
License     : GPL-3
Maintainer  : hello@mario.net.au
Stability   : experimental
Portability : POSIX

== Motivation

Helm is a batteries-included implementation of the [service pattern](https://www.schoolofhaskell.com/user/meiersi/the-service-pattern).

It eschews type class and mtl approaches (ala ['Scrap your type classes'](http://www.haskellforall.com/2012/05/scrap-your-type-classes.html)) to experiment with a more implicit value level approach.

It is intended to be used at the base level of your application, providing some structure for your IO-generating business logic.

== Service Types

Helm currently provides interfaces for the following types of services:

* Logger: for Debug, Info, Warning and Error level logging
* Channel: typed read/write channels with workers
* Database: basic RDBMS style querying
* SocketServer: websocket server

== Implementations

Helm provides the following implementations:

* Logger.StdOut: prints logged items to stdout
* Channel.Stm: A software-transactional-memory implementation of channels
* Helm.Database.Postgres: provides a couple of query functions while managing DB pool
* Helm.SocketServer

    * Helm.SocketServer.Echo: echoes back client responses without any handling
    * Helm.SocketServer.Hooked: takes extra function i.e. `onJoined name totalClients :: IO ()` that will be run for every client join

They can be used directly, or simply as reference code to pull out and create your own services as needed.

For example you may want a storage-backed channel that survives program restarts, so you might create your own, or implement Helm.Channel.DB which takes a Helm.Database handler for persistence.

== Quick Example

@
module Main where

import           Data.Monoid              ((<>))
import           Helm
import qualified Helm
import qualified Helm.Channel             as Channel
import qualified Helm.Channel.Stm         as Channel.Stm
import qualified Helm.Logger              as Logger
import qualified Helm.Logger.StdOut       as Logger.StdOut

main :: IO ()
main = Helm.manage $ do
  loggerH    <- Logger.StdOut.load
  broadcastH <- Channel.Stm.load

  Helm.program $ do
    Logger.logDebug loggerH "Handlers initiating..."

    -- Write our business logic here directly, i.e. logging all broadcast messages
    Channel.worker broadcastH (\text -> Logger.logDebug loggerH $ "broadcast-worker:" <> text)

    -- Or pass services off to some other function that implements the relevant business logic for our app
    run loggerH broadcastH
@
-}
module Helm
  ( -- * Running
    manage
  , program
  , manageTest)
  where

import Control.Concurrent    (threadDelay)
import Control.Monad         (forever)
import Control.Monad.Managed (Managed, MonadIO, liftIO, runManaged)
import System.IO (hSetBuffering, BufferMode(..), stdout)

-- | Wrapper for runManaged that than runs forever. See example above for usage.
manage :: forall a. Managed a -> IO ()
manage things =
  runManaged $ do
    -- Force LineBuffering for consistent output behavior
    liftIO $ hSetBuffering stdout LineBuffering
    liftIO $ putStrLn "Starting under Helm management..."
    _ <- things
    -- wait until the the process is killed
    forever $ liftIO $ threadDelay 100000

-- | TBC, for testing.
manageTest :: Managed () -> IO ()
manageTest = runManaged

-- | Utility function re-exporting liftIO to avoid Control.Monad.Managed import in Helm client code.
program :: MonadIO m => IO a -> m a
program = liftIO
