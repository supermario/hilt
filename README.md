<img align="right" src="https://mario.net.au/images/hilt-logo.svg" width="140">

:warning: Hilt is highly experimental and changing frequently.


**Hilt provides a set of batteries-included services for Haskell, and a way to use them together easily.**

It is intended to be used at the base level of your Haskell application, providing some structure for your business logic.


## How it works

An example app using the logging and channel services together. It simply logs any messages written to the channel.

```haskell
main :: IO ()
main = Hilt.manage $ do

  logger <- Logger.load
  chan   <- Channel.load

  Hilt.program $ do
    Logger.debug logger "Starting up!"

    Channel.worker chan (\text -> Logger.debug logger ("Received message: " <> text))

    Channel.write chan "Hello world!"

```
Hilt handles the underlying mechanics, threads, async behaviour, safety and service management/cleanup for us.

## Overview

Hilt currently provides the following types of services:

* [Cache](#cache): an in-memory key-value cache
* [Channel](#channel): typed read/write channels with workers
* [Logger](#logger): basic Debug, Info, Warning and Error level logging
* [Postgres](#postgres): postgres connection pool and querying
* [Websocket](#websocket): websocket server

You can use these services as-is, or as reference code to pull out and create your own services as needed. They are intended to be compact and easy to understand.

* []

## Cache

An in-memory key value cache. Contents are not persisted across app restarts.

`cache <- Cache.load`

##### Interface:

|            | Sample                                 | Description |
| -          | -                                      | -           |
| **insert** | `Cache.insert cache "mykey" "myvalue"` | Inserts a new key/value into the cache. |
| **lookup** | `value <- Cache.lookup cache "mykey"`  | Lookup a value by key. Returns a Maybe, as the item may not be found. |
| **delete** | `Cache.delete cache "mykey"`           | Deletes a given value, if the key exists. |
| **keys**   | `keys <- Cache.keys cache`             | Retrieves a list of all keys from the cache. |
| **size**   | `size <- Cache.size cache`             | Retrieves the size of the cache as an Int |

Cache currently requires both key and value to be type `Text`.

## Channel

A channel is a simple text based queue. You can write values to it, and read values from it. Once a value is read, it is no longer on the queue.

You might use a channel to pass messages between different parts of your app, or trigger actions in a seperate thread.

`chan <- Channel.load`


##### Interface:

|            | Sample                                 | Description |
| -          | -                                      | -           |
| write  | Writes a text value to the channel                                 | `Channel.write chan "Hello world!"` |
| read   | Waits to read a single text value from the channel                 | `text <- Channel.read chan` |
| worker | Fork a worker thread that runs given function for each read value. | `Channel.worker chan (\text -> Logger.debug logger ("Received message: " <> text))` |

You should only have one worker per channel, as messages can only be read once.


## Logger

An simple STDOUT logger.

`logger <- Logger.load`

##### Interface:

|            | Sample                                 | Description |
| -          | -                                      | -           |
| **log** | `Logger.debug logger "debug message"` | Writes `[Debug] debug message` to STDOUT |

You can also use `info`, `warning` and `error`.





## Postgres

An database service that handles connection pooling and configuration.

It expects a `DATABASE_URL` ENV var with a postgresql URL to be present, i.e. `postgres://user:pass@hostname:5432/databasename`.

`db <- Postgres.load`

##### Interface:

|            | Sample                                 | Description |
| -          | -                                      | -           |
| **queryMaybe** | `(res :: Maybe [Event]) <- Postgres.queryMaybe db "SELECT * FROM events"` | Execute a postgres query and decode the results to the specified type. |





## Websocket

A websocket server.

`websocket <- Websocket.load`

##### Interface:

|            | Sample                                 | Description |
| -          | -                                      | -           |
| **broadcast** | `Websocket.broadcast websocket "Hello world!"` | Sends the given text to all connected users |



## Setup

Hilt aims to have a bootstrap script eventually. Currently it requires the following steps on an existing Haskell stack project.

- In your `project.cabal` under the `executable` section
  - Add `hilt` to the `build-depends` list
  - Add `default-extensions: OverloadedStrings`
- In your `stack.yml`
  ```
  - location:
      git: https://github.com/supermario/hilt.git
      commit: 2a6381a
  ```
  Stack doesn't support a `master` target, so you'll need to pin the latest SHA until Hilt is released.


## Custom Services

Services are no more than an `IO` value wrapped by `Control.Monad.Managed`.

For example, if you wanted a storage-backed channel that survives program restarts, so you might:

* create your own service from scratch with baked in storage functionality
* or, create a `Hilt.Channel.DB` service and have it require a `Hilt.Handles.Database` service to use for persistence
* or, use an existing implementation you have and run it under [managed](https://hackage.haskell.org/package/managed) (for which [Hilt.managed](src/Hilt.hs#L85-L91) is just a wrapper), exposing a service interface

Services are very simple, just take a look at [Cache](src/Hilt/Cache.hs) for example, which just wraps the `Data.Cache` lib and provides an interface to it.



## Details

Hilt is an implementation of the [service pattern](https://www.schoolofhaskell.com/user/meiersi/the-service-pattern).

It intentionally avoids typeclass and monad-transformer approaches (ala ['Scrap your type classes'](http://www.haskellforall.com/2012/05/scrap-your-type-classes.html)) to experiment with a more explicit value-level approach.

It is intended to be used at the base level of your Haskell application, providing some structure for your IO-generating business logic.


<sub><sup>Logo by birdie brain from the Noun Project.</sup></sub>
