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
  mClientIds <- newTVarIO [1..]

  f Handle
    { send      = sendImpl mClients
    , broadcast = broadcastImpl mClients
    , app       = appImpl mClients mClientIds onJoined onReceive
    }

loadRaw :: OnJoined -> OnReceive -> IO Handle
loadRaw onJoined onReceive = do
  mClients <- newTVarIO []
  mClientIds   <- newTVarIO [1..]

  return Handle
    { send      = sendImpl mClients
    , broadcast = broadcastImpl mClients
    , app       = appImpl mClients mClientIds onJoined onReceive
    }

type ClientId = Int
type ClientIds = [Int]

type Client = (ClientId, WS.Connection)
type Clients = [Client]

type TClients = TVar Clients
type TClientIds = TVar ClientIds

-- OnJoined = totalClients -> IO (Maybe (response message))
type OnJoined = ClientId -> Int -> IO (Maybe T.Text)
-- OnReceive = receivedMessage -> -> IO ()
type OnReceive = ClientId -> T.Text -> IO ()

sendImpl :: TClients -> ClientId -> T.Text -> IO ()
sendImpl mClients clientId message = do
  clients <- atomically $ readTVar mClients
  send_ clients clientId message

broadcastImpl :: TClients -> T.Text -> IO ()
broadcastImpl mClients message = do
  clients <- atomically $ readTVar mClients
  broadcast_ clients message

appImpl :: TClients -> TClientIds -> OnJoined -> OnReceive -> WS.ServerApp
appImpl mClients mClientIds onJoined onReceive pendingConn = do
  conn <- WS.acceptRequest pendingConn
  WS.forkPingThread conn 30

  clientId <- atomically $ do
    clientIds <- readTVar mClientIds
    let next:remaining = clientIds
    writeTVar mClientIds remaining
    return next

  let client = (clientId, conn)
      disconnect = do
        atomically $ do
          clients <- readTVar mClients
          writeTVar mClients $ removeClient client clients

        T.putStrLn ("[websocket:disconnect] " <> T.pack (show clientId))
        return ()

  flip finally disconnect $ do
    clientCount <- atomically $ do
      clients <- readTVar mClients
      writeTVar mClients $ addClient client clients
      return $ length clients

    initText <- onJoined clientId clientCount
    case initText of
      Just text -> sendImpl mClients clientId text
      Nothing -> do
        -- @TODO PROBEL!M!
        putStrLn "[WARNING] No init message for new client was provided"
        return ()

    talk onReceive conn mClients client

talk :: OnReceive -> WS.Connection -> TClients -> Client -> IO ()
talk onReceive conn _ (clientId, _) = forever $ do
  msg <- WS.receiveData conn
  T.putStrLn ("[websocket:received] " <> T.pack (show clientId) <> ":" <> msg)
  onReceive clientId msg

addClient :: Client -> Clients -> Clients
addClient client clients = client : clients

removeClient :: Client -> Clients -> Clients
removeClient client = filter ((/= fst client) . fst)

findClient :: Clients -> ClientId -> Maybe Client
findClient clients clientId = find ((== clientId) . fst) clients

send_ :: Clients -> ClientId -> T.Text -> IO ()
send_ clients clientId text =
  case findClient clients clientId of
    Just (_, conn) -> WS.sendTextData conn text
    Nothing        -> return ()

broadcast_ :: Clients -> T.Text -> IO ()
broadcast_ clients message = do
  T.putStrLn ("[websocket:broadcast] " <> T.pack (show $ length clients) <> ":" <> message)
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message
