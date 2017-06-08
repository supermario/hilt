module Helm.SocketServer.Stm (load, loadDefault, withHandle) where

import Data.Monoid           ((<>))
import Control.Exception     (finally)
import Control.Monad         (forM_, forever)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import Control.Concurrent.STM

import qualified Helm.SocketServer as SocketServer

import Control.Monad.Managed (Managed, managed)

load :: OnJoined -> Managed SocketServer.Handle
load onJoined = managed $ withHandle onJoined

loadDefault :: OnJoined -> Managed SocketServer.Handle
loadDefault onJoined = managed $ withHandle onJoined

withHandle :: OnJoined -> (SocketServer.Handle -> IO a) -> IO a
withHandle onJoined f = do
  mClients <- newTVarIO []
  mNames   <- newTVarIO [1..]

  f SocketServer.Handle
    { SocketServer.broadcast = broadcastImpl mClients
    , SocketServer.app = appImpl mClients mNames onJoined
    }

-- Arguments represent;
-- OnJoined = name -> totalClients -> IO ()
type OnJoined = Int -> Int -> IO ()
type Client = (Int, WS.Connection)

type Clients = [Client]
type Names = [Int]

type TClients = TVar Clients
type TNames = TVar Names

broadcastImpl :: TClients -> T.Text -> IO ()
broadcastImpl mClients message = do
  clients <- atomically $ readTVar mClients
  broadcast message clients

appImpl :: TClients -> TNames -> OnJoined -> WS.ServerApp
appImpl mClients mNames onJoined pendingConn = do
  conn <- WS.acceptRequest pendingConn
  WS.forkPingThread conn 30

  name <- atomically $ do
    names <- readTVar mNames
    let n:remaining = names
    writeTVar mNames remaining
    return n

  let client = (name, conn)
      disconnect = do
        atomically $ do
          clients <- readTVar mClients
          writeTVar mClients $ removeClient client clients

        T.putStrLn ("[Debug] SocketServer:disconnect:" <> T.pack (show name))
        return ()

  flip finally disconnect $ do
    clientCount <- atomically $ do
      clients <- readTVar mClients
      writeTVar mClients $ addClient client clients
      return $ length clients

    _ <- onJoined name clientCount

    talk conn mClients client

talk :: WS.Connection -> TClients -> Client -> IO ()
-- talk conn mClients (user, _) = forever $ do
talk conn _ (user, _) = forever $ do
  msg <- WS.receiveData conn
  T.putStrLn ("[Debug] SocketServer:received:" <> T.pack (show user) <> ":" <> msg)
  -- readTVar mClients >>= broadcast (T.pack (show user) <> ":" <> msg)

addClient :: Client -> Clients -> Clients
addClient client clients = client : clients

removeClient :: Client -> Clients -> Clients
removeClient client = filter ((/= fst client) . fst)

broadcast :: T.Text -> Clients -> IO ()
broadcast message clients = do
  T.putStrLn ("[Debug] SocketServer:broadcast:" <> T.pack (show $ length clients) <> ":" <> message)
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message
