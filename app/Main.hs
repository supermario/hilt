module Main where

import           Data.Monoid        ((<>))
import           Helm
import qualified Helm
import qualified Helm.Channel       as Channel
import qualified Helm.Channel.Stm   as Channel.Stm
import qualified Helm.Logger        as Logger
import qualified Helm.Logger.StdOut as Logger.StdOut

main :: IO ()
main =
    Helm.manage $
    do loggerH <- Logger.StdOut.load
       broadcastH <- Channel.Stm.load
       Helm.program $
           do Logger.logDebug loggerH "Handlers initiating..."
              Channel.worker
                  broadcastH
                  (\text ->
                        Logger.logDebug loggerH $ "ws-broadcast-worker:" <> text)
              Logger.logDebug loggerH "Up and running..."
