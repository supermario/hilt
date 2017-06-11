:warning: Hilt is still in experimental status, and should not yet be used aside from as a code reference.

## Motivation

Hilt is a batteries-included implementation of the [service pattern](https://www.schoolofhaskell.com/user/meiersi/the-service-pattern).

It eschews typeclass and mtl approaches (ala ['Scrap your type classes'](http://www.haskellforall.com/2012/05/scrap-your-type-classes.html)) to experiment with a more explicit value level approach.

It is intended to be used at the base level of your Haskell application, providing some structure for your IO-generating business logic. See the Quick Example below to get a feel for what that looks like in practice.

## Service Types

Hilt currently provides interfaces for the following types of services:

* [Logger](src/Hilt/Logger.hs): for Debug, Info, Warning and Error level logging
* [Channel](src/Hilt/Channel.hs): typed read/write channels with workers
* [Database](src/Hilt/Database.hs): basic RDBMS style querying
* [SocketServer](src/Hilt/SocketServer.hs): websocket server

Each of these defines a `Handle` type. For example, an implementation of `Hilt.Logger` will provide a `load :: Managed Hilt.Logger.Handle`

This way client code only needs `import Hilt.Logger`, and the top-level `main` will select which implementation is used, perhaps `Hilt.Logger.StdOut` for production and some hypothetical `Hilt.Logger.Silent` for tests.

See example below for an overview.

## Implementations

Hilt currently provides the following implementations of the Service Types:

### Logger
* [Hilt.Logger.StdOut](src/Hilt/Logger/StdOut.hs): prints logged items to stdout

### Channel
* [Hilt.Channel.Stm](src/Hilt/Channel/Stm.hs): A software-transactional-memory implementation of channels, using `Control.Concurrent.STM.TChan`
* [Hilt.Channel.Stm](src/Hilt/Channel/Stm.hs): A faster implementation of channels using the [unagi-chan](https://github.com/jberryman/unagi-chan) library

### Database
* [Hilt.Database.Postgres](src/Hilt/Database/Postgres.hs): provides a couple of query functions while managing DB pool

### SocketServer
* [Hilt.SocketServer.Echo](src/Hilt/SocketServer/Echo.hs): echoes back client responses without any handling
* [Hilt.SocketServer.Hooked](src/Hilt/SocketServer/Hooked.hs): takes extra function i.e. `onJoined name totalClients :: Int -> Int -> IO ()` that will be run for every client join
* [Hilt.SocketServer.Stm](src/Hilt/SocketServer/Hooked.hs): a software-transactional-memory version of `Hilt.SocketServer.Hooked`.

They can be used directly, or simply as reference code to pull out and create your own services as needed, they are intended to be compact and easy to understand.

For example you may want a storage-backed channel that survives program restarts, so you might:

* create your own with baked in storage functionality
* create `Hilt.Channel.DB` and have it depend on some `Hilt.Database` service for persistence
* use an existing service and run it under [managed](https://hackage.haskell.org/package/managed) (for which [Hilt.managed](src/Hilt.hs#L85-L91) is just a wrapper)

## Quick Example

```haskell
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
```
