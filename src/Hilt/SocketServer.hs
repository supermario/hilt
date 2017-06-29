module Hilt.SocketServer (module Hilt.SocketServer, module Hilt.Handles.SocketServer) where

import Data.Monoid           ((<>))
import Data.List             (find)
import Control.Exception     (finally)
import Control.Monad         (forM_, forever)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import Control.Concurrent.STM

import Hilt.Handles.SocketServer

-- @TODO all logging should go via handler dependency so user can control behavior

import Control.Monad.Managed (Managed, managed)

load :: OnJoined -> OnReceive -> Managed Handle
load onJoined onReceive = managed $ withHandle onJoined onReceive

loadDefault :: OnJoined -> OnReceive -> Managed Handle
loadDefault onJoined onReceive = managed $ withHandle onJoined onReceive

withHandle :: OnJoined -> OnReceive -> (Handle -> IO a) -> IO a
withHandle onJoined onReceive f = do
  mClients <- newTVarIO []
  mNames   <- newTVarIO [1..]

  f Handle
    { send      = sendImpl mClients
    , broadcast = broadcastImpl mClients
    , app       = appImpl mClients mNames onJoined onReceive
    }


type Name = Int
type Names = [Int]

type Client = (Name, WS.Connection)
type Clients = [Client]

type TClients = TVar Clients
type TNames = TVar Names

-- OnJoined = totalClients -> IO (Maybe (response message))
type OnJoined = Int -> IO (Maybe T.Text)
-- OnReceive = receivedMessage -> -> IO ()
type OnReceive = T.Text -> IO ()

sendImpl :: TClients -> Name -> T.Text -> IO ()
sendImpl mClients name message = do
  clients <- atomically $ readTVar mClients
  send_ clients name message

broadcastImpl :: TClients -> T.Text -> IO ()
broadcastImpl mClients message = do
  clients <- atomically $ readTVar mClients
  broadcast_ clients message

appImpl :: TClients -> TNames -> OnJoined -> OnReceive -> WS.ServerApp
appImpl mClients mNames onJoined onReceive pendingConn = do
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

    initText <- onJoined clientCount
    case initText of
      Just text -> sendImpl mClients name text
      Nothing -> return ()

    talk onReceive conn mClients client

talk :: OnReceive -> WS.Connection -> TClients -> Client -> IO ()
talk onReceive conn _ (user, _) = forever $ do
  msg <- WS.receiveData conn
  T.putStrLn ("[Debug] SocketServer:received:" <> T.pack (show user) <> ":" <> msg)
  onReceive msg

addClient :: Client -> Clients -> Clients
addClient client clients = client : clients

removeClient :: Client -> Clients -> Clients
removeClient client = filter ((/= fst client) . fst)

findClient :: Clients -> Name -> Maybe Client
findClient clients name = find ((== name) . fst) clients

send_ :: Clients -> Name -> T.Text -> IO ()
send_ clients name text =
  case findClient clients name of
    Just (_, conn) -> WS.sendTextData conn text
    Nothing        -> return ()

broadcast_ :: Clients -> T.Text -> IO ()
broadcast_ clients message = do
  T.putStrLn ("[Debug] SocketServer:broadcast:" <> T.pack (show $ length clients) <> ":" <> message)
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message
