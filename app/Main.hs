module Main where

import           TextShow
import           Data.Monoid        ((<>))
import qualified Hilt
import qualified Hilt.Server
import qualified Hilt.Channel      as Channel
import qualified Hilt.Logger       as Logger
import qualified Hilt.SocketServer as Websocket

{-
To run this sample locally:

```
git clone git@github.com:supermario/hilt.git
cd hilt && stack build && stack exec hilt-example

# In another terminal window
curl -i -N -H "Connection: Upgrade" -H "Upgrade: websocket" -H "Sec-WebSocket-Key: SGVsbG8sIHdvcmxkIQ==" localhost:8081/ws
```

The Hilt.Postgres service is not demonstrated as it needs an existing Postgres DB in the DATABASE_URL ENV var,
and table to query. The addition is simple however if you want it:

  -- Add the db service next to the other services
  db <- Hilt.Postgres.load

  -- Use the handle within your program
  Hilt.Postgres.query db "SELECT * FROM myTable WHERE x = ?" ["1"]

-}
main :: IO ()
main = Hilt.manage $ do

  logger <- Logger.load
  chan   <- Channel.load

  let onJoined :: Websocket.OnJoined
      onJoined clientId clientCount = do
        Logger.debug logger $ showt clientId <> " joined, " <> showt clientCount <> " connected."
        pure $ Just "Hello client!"

      onReceive :: Websocket.OnReceive
      onReceive clientId text = do
        Logger.debug logger $ showt clientId <> " said " <> showt text
        Channel.write chan text

  websocket <- Websocket.load onJoined onReceive

  -- Now we can write our business logic using our services
  Hilt.program $ do
    Logger.debug logger "Starting up!"

    -- Log all messages received, then broadcast back to all clients
    let workHandler text = do
          Logger.debug logger $ "[worker] got " <> text
          Websocket.broadcast websocket text


    -- Run our worker and our websocket server
    Channel.worker chan workHandler
    Hilt.Server.runWebsocket websocket

    -- Now we can pass services off to some other areas of our app
    -- someMoreLogic logger chan

    -- Or we can just use them here
    Channel.write chan "Hello world!"
