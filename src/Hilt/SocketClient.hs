module Hilt.SocketClient where

import           Control.Monad       (forever)
import           Network.Socket      (withSocketsDo)
import qualified Data.Text           as T
import qualified Network.WebSockets  as WS
import           Wuss                (runSecureClient)
import           Control.Concurrent.Chan

handleToChan :: (WS.WebSocketsData a) => Chan a -> WS.ClientApp()
handleToChan chan connection = do
  _ <- forever $ do
    msg <- WS.receiveData connection
    writeChan chan msg

  WS.sendClose connection ("bye from handler!" :: T.Text)

-- Given a channel, host and path
-- Start a secure websocket client on host and path,
-- and pipe all server messages to the channel
websocketClientRunResponseChannel :: (WS.WebSocketsData a) => Chan a -> String -> String -> IO()
websocketClientRunResponseChannel chan host path = withSocketsDo $ runSecureClient host 443 path f
  where f = handleToChan chan
