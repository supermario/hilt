:warning: Helm is still in experimental status, and should not yet be used aside from as a code reference.

## Motivation

Helm is a batteries-included implementation of the [service pattern](https://www.schoolofhaskell.com/user/meiersi/the-service-pattern).

It eschews typeclass and mtl approaches (ala ['Scrap your type classes'](http://www.haskellforall.com/2012/05/scrap-your-type-classes.html)) to experiment with a more explicit value level approach.

It is intended to be used at the base level of your Haskell application, providing some structure for your IO-generating business logic. See the Quick Example below to get a feel for what that looks like in practice.

## Service Types

Helm currently provides interfaces for the following types of services:

* [Logger](src/Helm/Logger.hs): for Debug, Info, Warning and Error level logging
* [Channel](src/Helm/Channel.hs): typed read/write channels with workers
* [Database](src/Helm/Database.hs): basic RDBMS style querying
* [SocketServer](src/Helm/SocketServer.hs): websocket server

Each of these defines a `Handle` type. For example, an implementation of `Helm.Logger` will provide a `load :: Managed Helm.Logger.Handle`

This way client code only needs `import Helm.Logger`, and the top-level `main` will select which implementation is used, perhaps `Helm.Logger.StdOut` for production and some hypothetical `Helm.Logger.Silent` for tests.

See example below for an overview.

## Implementations

Helm currently provides the following implementations:

* [Helm.Logger.StdOut](src/Helm/Logger/StdOut.hs): prints logged items to stdout
* [Helm.Channel.Stm](src/Helm/Channel/Stm.hs): A software-transactional-memory implementation of channels
* [Helm.Database.Postgres](src/Helm/Database/Postgres.hs): provides a couple of query functions while managing DB pool
* [Helm.SocketServer.Echo](src/Helm/SocketServer/Echo.hs): echoes back client responses without any handling
* [Helm.SocketServer.Hooked](src/Helm/SocketServer/Hooked.hs): takes extra function i.e. `onJoined name totalClients :: Int -> Int -> IO ()` that will be run for every client join

They can be used directly, or simply as reference code to pull out and create your own services as needed, they are intended to be compact and easy to understand.

For example you may want a storage-backed channel that survives program restarts, so you might:

* create your own with baked in storage functionality
* create `Helm.Channel.DB` and have it depend on some `Helm.Database` service for persistence
* use an existing service and run it under [managed](https://hackage.haskell.org/package/managed) (for which [Helm.managed](https://github.com/supermario/helm/blob/master/src/Helm.hs#L85-L91) is just a wrapper)

## Quick Example

```haskell
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
```
