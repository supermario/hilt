module Helm (manage, program, manageTest, someFunc) where

import Control.Monad.Managed (runManaged, liftIO, Managed, MonadIO)
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

-- Utility function to avoid Control.Monad.Managed imports
program :: MonadIO m => IO a -> m a
program = liftIO
