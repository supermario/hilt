{-# LANGUAGE OverloadedStrings #-}

module Helm.Socket where

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

websocketRunChannel :: (WS.WebSocketsData a) => Chan a -> String -> String -> IO()
websocketRunChannel chan host path = withSocketsDo $ runSecureClient host 443 path f
  where f = handleToChan chan
