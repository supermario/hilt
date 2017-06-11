module Main where

import           Data.Monoid        ((<>))
import           Hilt
import qualified Hilt
import qualified Hilt.Channel       as Channel
import qualified Hilt.Channel.Stm   as Channel.Stm
import qualified Hilt.Logger        as Logger
import qualified Hilt.Logger.StdOut as Logger.StdOut

main :: IO ()
main =
    Hilt.manage $
    do loggerH <- Logger.StdOut.load
       broadcastH <- Channel.Stm.load
       Hilt.program $
           do Logger.logDebug loggerH "Handlers initiating..."
              Channel.worker
                  broadcastH
                  (\text ->
                        Logger.logDebug loggerH $ "ws-broadcast-worker:" <> text)
              Logger.logDebug loggerH "Up and running..."
