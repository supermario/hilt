module Helm (manage, manageTest, someFunc) where

import Control.Monad.Managed (runManaged, liftIO)
import Control.Concurrent    (threadDelay)
import Control.Monad         (forever)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

manage things = do
  runManaged $ do
    liftIO $ putStrLn "Starting under Helm management..."
    things
    -- wait until the the process is killed
    forever $ liftIO $ threadDelay 100000

--
manageTest things = do
  runManaged $ do
    things
