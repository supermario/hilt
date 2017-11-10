<img align="right" src="https://mario.net.au/images/hilt-logo.svg" width="140">

**Hilt provides a set of opinionated batteries-included services for Haskell, and a way to use them together easily, allowing you to get the handle of Haskell.**

It is intended to be used at the base level of your Haskell application, providing some structure for your business logic.

:warning: Hilt is still alpha, questions/feedback welcome

## Table of Contents

- [Example](#example)
- [Setup](#setup)
- [Services](#services)
  - [Logger](#Logger)
  - [Websocket](#Websocket)
  - [Postgres](#Postgres)
  - [Channel](#Channel)
  - [Cache](#Cache)
- [Helpers](#helpers)
- [Custom Services](#custom-services)
- [Implementation Details](#implementation-details)


## Example

A basic example using the `Logger` and `Channel` services together. It simply writes any messages written to the channel, which the worker logs.

```haskell
main = Hilt.manage $ do

  logger <- Logger.load
  chan   <- Channel.load

  Hilt.program $ do
    Logger.debug logger "Starting up!"

    Channel.worker chan (\text -> Logger.debug logger ("Received message: " <> text))

    Channel.write chan "Hello world!"
    Channel.write chan "Goodbye world!"
```

Hilt handles the underlying mechanics, threads, async behaviour, safety and service management/cleanup for us.

For a full, runnable example, see [app/Main.hs](app/Main.hs).


## Setup

With the Haskell tool [`stack`](https://github.com/commercialhaskell/stack) installed;

- Create a new project with `stack new <projectname> new-template`, or adjust the `main` of an existing one
- In your `project.cabal` under the `executable` section
  - Add `hilt` to the `build-depends` list
  - Add `default-extensions: OverloadedStrings`
- In your `stack.yml` either add or merge the following settings:
  ```yaml
  packages:
  - location:
      git: https://github.com/supermario/hilt.git
      commit: efa1540a0c914f4277d9e104f93fa7244f540652 # Current Master Sha
  ```
  Stack doesn't support a `master` target, so you'll need to pin the latest SHA until Hilt is released.


## Services

Hilt currently provides the following types of services:

* [`Logger`](#logger): basic Debug, Info, Warning and Error level logging service
* [`Websocket`](#websocket): websocket service
* [`Postgres`](#postgres): postgres connection pool and querying service
* [`Channel`](#channel): typed read/write channel service with workers
* [`Cache`](#cache): an in-memory key-value cache service

You can use these services as-is, or as reference code to pull out and create your own services as needed – each one is contained in a single file. They are intended to be compact and easy to understand.

Hilt also provides some helpers; [`Config`](#config), [`JSON`](#json) and [`Server`](#server).

### Logger

An simple STDOUT logger.

##### Interface:

Create a handle with `logger <- Logger.load` and then:

|            | Usage                                  | Description |
| -          | -                                      | -           |
| **log** | `Logger.debug logger "debug message"` | Writes `[Debug] debug message` to STDOUT |

In addition to `.debug` You can also use `.info`, `.warning` and `.error`.




### Websocket

This service has a couple of moving parts. We need to

- specify what we want to for `onJoined` and `onReceive` events
- boot the HTTP server.

##### Interface:

Here's a simple example that just logs joins and receives and sends no messages:

```haskell
main = Hilt.manage $ do
  logger <- Logger.load

  let
    onJoined clientId clientCount = do
      Logger.debug logger $ showt clientId <> " joined, " <> showt clientCount <> " connected."
      return Nothing

    onReceive clientId text =
      Logger.debug logger $ showt clientId <> " said " <> showt text

  websocket <- Websocket.load onJoined onReceive

  Hilt.program $ do
    -- Start a HTTP server on port 8081
    Hilt.Server.runWebsocket websocket
```

Your program logic can now use the `websocket` handle to:

|            | Usage                                  | Description |
| -          | -                                      | -           |
| **send** | `Websocket.send websocket clientId "Hello world!"` | Send a message to a single client |
| **broadcast** | `Websocket.broadcast websocket "Hello world!"` | Send a message to all clients |

For a full, runnable example, see [app/Main.hs](app/Main.hs).




### Postgres

A database service that handles connection pooling and configuration.

It expects a `DATABASE_URL` ENV var with a postgresql URL to be present, i.e. `DATABASE_URL=postgres://user:pass@hostname:5432/databasename`.

##### Interface:

Create a handle with `db <- Postgres.load` and then:

|            | Usage                                  | Description |
| -          | -                                      | -           |
| **query_** | `events :: [Event] <- Postgres.query_ db "SELECT * FROM events"` | Run a [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple/docs/Database-PostgreSQL-Simple.html) query and decode the results to the specified type. |
| **query** | `events :: [Event] <- Postgres.query db "SELECT * FROM ?" ["events"]` | Run a paramaterised [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple/docs/Database-PostgreSQL-Simple.html) query and decode the results to the specified type. Params interpolate into `?` within the query. Use a singleton array `[a]` for a single param and tuples `(a,b,...)` for multiple params |
| **execute** | `Postgres.execute db "DROP TABLE events" ()` | Run a [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple/docs/Database-PostgreSQL-Simple.html) query that returns no results. Params interpolate into `?` within the query. Use a singleton array `[a]` for a single param and tuples `(a,b,...)` for multiple params or unit `()` for none |
| **queryP**   | `events :: [Event] <- Postgres.exec db $ selectList [] []` | Runs a persistent query. See the [persistent guide](http://www.yesodweb.com/book/persistent). |
| **queryMaybe** (TBC) | `events :: Maybe [Event] <- Postgres.queryMaybe db "SELECT * FROM events"` | Future idea; execute a postgres query and decode the results to the specified type. |

The interface is rather raw currently, simply exposing certain parts of the underlying libraries to get a feel for the right approach.

Also connection pooling currently only works for the `queryP` function.

In future they might tend more towards something like `queryMaybe` interface described.





### Cache

An in-memory key value cache. Contents are not persisted across app restarts.

##### Interface:

Create a handle with `cache <- Cache.load` and then:

|            | Usage                                  | Description |
| -          | -                                      | -           |
| **insert** | `Cache.insert cache "mykey" "myvalue"` | Inserts a new key/value into the cache |
| **lookup** | `value <- Cache.lookup cache "mykey"`  | Lookup a value by key. Returns a Maybe, as the item may not be found |
| **delete** | `Cache.delete cache "mykey"`           | Deletes a given value, if the key exists |
| **keys**   | `keys <- Cache.keys cache`             | Retrieves a list of all keys from the cache |
| **size**   | `size <- Cache.size cache`             | Retrieves the size of the cache as an Int |

Cache currently requires both key and value to be type [`Text`](https://hackage.haskell.org/package/text/docs/Data-Text.html). It will be extended to support any value type in future.



### Channel

A channel is a simple text based queue. You can write values to it, and read values from it. Once a value is read, it is no longer on the queue.

You might use a channel to pass messages between different parts of your app, or trigger actions in a seperate thread. See [app/Main.hs](app/Main.hs) for an example.

##### Interface:

Create a handle with `chan <- Channel.load` and then:

|            | Usage                                  | Description |
| -          | -                                      | -           |
| **write**  | `Channel.write chan "Hello world!"` | Writes a text value to the channel |
| **read**   | `text <- Channel.read chan` | Waits to read a single text value from the channel |
| **worker** | `Channel.worker chan (\text -> Logger.debug logger ("Received message: " <> text))` | Fork a worker thread that runs given function for each read value |

Generally you should only have one worker per channel, as messages can only be read once.


## Helpers

### Server

`runWebsocket :: Websocket.Handle -> IO ()`

Runs a Wai app on Warp with the provided `Websocket` handle. Also serves any static files from `/public`.

There will be `runHttp` and `runWebsocketAndHttp` in future.

#### Middlewares

A number of common middlewares are provided, see [Server.hs](https://github.com/supermario/hilt/blob/master/src/Hilt/Server.hs#L43).


## Custom Services

Hilt services are no more than an `IO` value wrapped by [`Control.Monad.Managed`](https://hackage.haskell.org/package/managed).

For example, if you wanted to write your own Hilt service for a storage-backed channel that survives program restarts, you might:

* create your own channel service from scratch with baked in storage functionality
* or, create a `Hilt.Channel.DB` service and have it require a `Hilt.Handles.Postgres` service to use for persistence
* or, use an existing implementation you have and run it under [managed](https://hackage.haskell.org/package/managed) (for which [Hilt.managed](src/Hilt.hs#L85-L91) is just a wrapper), exposing a service interface

Services are very simple, take a look at [Cache](src/Hilt/Cache.hs) for example, which just wraps the `Data.Cache` lib as-is.



## Implementation Details

Hilt is an implementation of the [service pattern](https://www.schoolofhaskell.com/user/meiersi/the-service-pattern).

It intentionally avoids typeclass and monad-transformer approaches (ala ['Scrap your type classes'](http://www.haskellforall.com/2012/05/scrap-your-type-classes.html)) to experiment with a more explicit value-level approach.

It is intended to be used at the base level of your Haskell application, providing some structure for your IO-generating business logic.

Thread management is rather carefree presently and will be improved in future.


<sub><sup>Logo by birdie brain from the Noun Project.</sup></sub>
