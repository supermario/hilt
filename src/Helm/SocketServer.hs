{-# LANGUAGE OverloadedStrings #-}

module Helm.SocketServer where

import qualified Data.Text as T
import qualified Network.WebSockets as WS

data Handle = Handle
  { broadcast :: T.Text -> IO ()
  , app :: WS.ServerApp
  }
