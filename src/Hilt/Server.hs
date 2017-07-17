module Hilt.Server where

import Control.Concurrent (forkIO)

import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai                    (responseLBS)
import Network.HTTP.Types             (status404)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets             (defaultConnectionOptions)

import qualified Hilt.Config as Config
import qualified Hilt.SocketServer as SocketServer


{- Fork a thread and boot the socket server as a Wai app on Warp -}
runWebsocket :: SocketServer.Handle -> IO ()
runWebsocket socketHandle = do
  port <- Config.lookupEnv "PORT" 8081

  -- @TODO serve static asset middleware
  let backupApp _ respond = respond $ responseLBS status404 [] "Not found."
      waiApp = websocketsOr defaultConnectionOptions (SocketServer.app socketHandle) backupApp

  _ <- forkIO $ Warp.run port waiApp

  return ()

-- runHttp @TODO
-- runWebsocketAndHttp @TODO
