module Hilt.SocketServer where

import qualified Data.Text as T
import qualified Network.WebSockets as WS

data Handle = Handle
  { send      :: Int -> T.Text -> IO ()
  , broadcast :: T.Text -> IO ()
  , app       :: WS.ServerApp
  }
