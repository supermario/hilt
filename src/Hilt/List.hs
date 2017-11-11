module Hilt.List where

import System.Random
import Data.Array.IO
import Control.Monad

-- https://wiki.haskell.org/Random_shuffle
-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
  ar <- newArray n xs
  forM [1 .. n] $ \i -> do
    j  <- randomRIO (i, n)
    vi <- readArray ar i
    vj <- readArray ar j
    writeArray ar j vi
    pure vj
 where
  n = length xs
  newArray :: Int -> [a] -> IO (IOArray Int a)
  newArray n = newListArray (1, n)
