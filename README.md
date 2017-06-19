<img align="right" src="https://mario.net.au/images/hilt-logo.svg" width="140">

:warning: Hilt is still in experimental status


**Hilt aims to provide a set of batteries-included services for Haskell, and a way to use them together easily.**

It is intended to be used at the base level of your Haskell application, providing some structure for your business logic.

```haskell
main :: IO ()
main = Hilt.manage $ do

  logger <- Logger.load
  chan   <- Channel.load

  Hilt.program $ do
    Logger.debug logger "Starting up!"

    Channel.worker chan (\text -> Logger.debug logger ("Received message: " <> text))

    Channel.write chan "Hello world!"

    -- Or pass services off to some other areas of your app
    someMoreLogic logger chan
```

Hilt handles the underlying mechanics, threads, async behaviour, safety and service management/cleanup for us.


## Service Types

Hilt currently provides the following types of services:

* [Cache](src/Hilt/Cache.hs): an in-memory key-value cache
* [Channel](src/Hilt/Channel.hs): typed read/write channels with workers
* [Logger](src/Hilt/Logger.hs): basic Debug, Info, Warning and Error level logging
* [Postgres](src/Hilt/Postgres.hs): postgres connection pool and querying
* [Websocket](src/Hilt/Websocket.hs): websocket server


You can use these services as-is, or as reference code to pull out and create your own services as needed. They are intended to be compact and easy to understand.

## Details

Hilt is an implementation of the [service pattern](https://www.schoolofhaskell.com/user/meiersi/the-service-pattern).

It intentionally avoids typeclass and mtl approaches (ala ['Scrap your type classes'](http://www.haskellforall.com/2012/05/scrap-your-type-classes.html)) to experiment with a more explicit value-level approach.

It is intended to be used at the base level of your Haskell application, providing some structure for your IO-generating business logic.


## Custom Services

Services are simply any `IO` code wrapped in a `Control.Monad.Managed` value type.

For example, if you wanted a storage-backed channel that survives program restarts, so you might:

* create your own service from scratch with baked in storage functionality
* or, create a `Hilt.Channel.DB` service and have it require a `Hilt.Handles.Database` service to use for persistence
* or, use an existing implementation you have and run it under [managed](https://hackage.haskell.org/package/managed) (for which [Hilt.managed](src/Hilt.hs#L85-L91) is just a wrapper), exposing a service interface

Services are very simple, just take a look at [Cache](src/Hilt/Cache.hs) for example, which just wraps the `Data.Cache` lib and provides an interface to it.


<sub><sup>Logo by birdie brain from the Noun Project.</sup></sub>
