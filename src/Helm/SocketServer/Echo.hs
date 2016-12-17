module Helm.SocketServer.Echo (load, withHandle) where

import Data.Char (isPunctuation, isSpace)
import Data.Monoid ((<>))
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Network.WebSockets as WS

import qualified Helm.SocketServer as SocketServer
import qualified Helm.Logger       as Logger
import qualified Helm.Channel      as Channel

import System.Random

import Control.Monad.Managed (managed)

load introText = managed $ withHandle introText

withHandle :: T.Text -> (SocketServer.Handle -> IO a) -> IO a
withHandle introText f = do

  mClients <- wsNewClients

  f SocketServer.Handle
    { SocketServer.broadcast = broadcastImpl mClients
    , SocketServer.app = taggedApplication mClients introText
    }


broadcastImpl :: MVar ServerState -> T.Text -> IO ()
broadcastImpl mClients message = do
  clients <- readMVar mClients
  broadcast message clients


taggedApplication :: MVar ServerState -> T.Text -> WS.ServerApp
taggedApplication mState introText pendingConn = do
  conn <- WS.acceptRequest pendingConn
  WS.forkPingThread conn 30

  WS.sendTextData conn introText

  msg     <- WS.receiveData conn
  clients <- readMVar mState

  case msg of
    _ | not (prefix `T.isPrefixOf` msg) ->
        sendText "Error: Unexpected announcement, expecting 'register:<uuid>'"

      | any ($ fst client) [T.null, T.any isPunctuation, T.any isSpace] ->
        sendText $ "Error: User identifier cannot contain punctuation or whitespace, and cannot be empty"

      | clientExists client clients ->
          sendText "Error: User already exists"

      | otherwise -> flip finally disconnect $ do
         modifyMVar_ mState $ \s -> do
           -- broadcast (fst client <> " joined") s'
           return $ addClient client s

         talk conn mState client

      where
        prefix     = "register:"
        client     = (T.drop (T.length prefix) msg, conn)
        disconnect = do
          -- Remove client and return new state
          s <- modifyMVar mState $ \s ->
            let s' = removeClient client s in return (s', s')
          return ()
        sendText t = WS.sendTextData conn (t :: Text)



type Client = (Text, WS.Connection)
type ServerState = [Client]

talk :: WS.Connection -> MVar ServerState -> Client -> IO ()
talk conn state (user, _) = forever $ do
  msg <- WS.receiveData conn
  readMVar state >>= broadcast
    (user <> ": " <> msg)

wsNewClients :: IO (MVar ServerState)
wsNewClients = newMVar newServerState

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
  T.putStrLn ("Broadcasting:" <> message)
  putStrLn $ "Broadcasting to " ++ (show $ length clients)
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message
