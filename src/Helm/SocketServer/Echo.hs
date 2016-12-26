module Helm.SocketServer.Echo (load, withHandle) where

import Data.Monoid           ((<>))
import Control.Exception     (finally)
import Control.Monad         (forM_, forever)
import Control.Concurrent    (MVar, newMVar, modifyMVar_, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

import qualified Helm.SocketServer as SocketServer

import Control.Monad.Managed (Managed, managed)

load :: Managed SocketServer.Handle
load = managed $ withHandle

withHandle :: (SocketServer.Handle -> IO a) -> IO a
withHandle f = do
  mClients <- newMVar []
  mNames   <- newMVar [1..]

  f SocketServer.Handle
    { SocketServer.broadcast = broadcastImpl mClients
    , SocketServer.app = appImpl mClients mNames
    }

type Client = (Int, WS.Connection)
type ServerState = [Client]
type Names = [Int]

broadcastImpl :: MVar ServerState -> T.Text -> IO ()
broadcastImpl mClients message = do
  clients <- readMVar mClients
  broadcast message clients

appImpl :: MVar ServerState -> MVar Names -> WS.ServerApp
appImpl mState mNames pendingConn = do
  conn <- WS.acceptRequest pendingConn
  WS.forkPingThread conn 30

  name:names <- readMVar mNames
  modifyMVar_ mNames $ \_ -> return names

  let client = (name, conn)
      disconnect = modifyMVar_ mState $ \s -> return $ removeClient client s

  flip finally disconnect $ do
    modifyMVar_ mState $ \s -> return $ addClient client s

    talk conn mState client

talk :: WS.Connection -> MVar ServerState -> Client -> IO ()
talk conn state (user, _) = forever $ do
  msg <- WS.receiveData conn
  readMVar state >>= broadcast
    ((T.pack $ show user) <> ":" <> msg)

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcast :: T.Text -> ServerState -> IO ()
broadcast message clients = do
  T.putStrLn ("[Debug] SocketServer:broadcast:" <> T.pack (show $ length clients) <> ":" <> message)
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message
