module Hilt.Server where

import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai                    (responseLBS)
import Network.HTTP.Types             (status404)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets             (defaultConnectionOptions)

import qualified Hilt.Config as Config
import qualified Hilt.SocketServer as SocketServer

runWebsocket :: SocketServer.Handle -> IO ()
runWebsocket socketHandle = do
  port <- Config.lookupEnv "PORT" 8081

  -- @TODO serve static asset middleware
  let backupApp _ respond = respond $ responseLBS status404 [] "Not found."
      waiApp = websocketsOr defaultConnectionOptions (SocketServer.app socketHandle) backupApp

  Warp.run port waiApp

-- runHttp @TODO
-- runWebsocketAndHttp @TODO
