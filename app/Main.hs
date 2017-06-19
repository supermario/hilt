module Main where

import           Data.Monoid        ((<>))
import qualified Hilt
import qualified Hilt.Channel as Channel
import qualified Hilt.Logger  as Logger

main :: IO ()
main = Hilt.manage $ do

  logger <- Logger.load
  chan   <- Channel.load

  -- Now we can write our business logic using our services
  Hilt.program $ do
    Logger.debug logger "Starting up!"

    Channel.worker chan (\text -> Logger.debug logger ("Received message: " <> text))

    Channel.write chan "Hello world!"

    -- Or pass services off to some other areas of your app
    -- someMoreLogic logger chan
