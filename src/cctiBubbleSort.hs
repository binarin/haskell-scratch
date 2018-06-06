{-

https://www.hackerrank.com/challenges/ctci-bubble-sort/problem

-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Array.IO
import Data.Array
import Control.Monad
import Data.IORef

bubbleSort :: Array Int Int -> IO (Int, Array Int Int)
bubbleSort arr = do
  ctr :: IORef Int <- newIORef 0
  mut :: IOArray Int Int <- thaw arr
  (low, high) <- getBounds mut
  forM_ [low..high] $ \_ -> do
    forM_ [low..high-1] $ \j -> do
      this <- readArray mut j
      next <- readArray mut (j + 1)
      when (this > next) $ do
        writeArray mut j next
        writeArray mut (j + 1) this
        modifyIORef' ctr (+1)
  (,) <$> readIORef ctr <*> freeze mut

main :: IO()
main = do
    n <- readLn :: IO Int

    aTemp <- getLine

    let a = fmap (read :: String -> Int) . words $ aTemp
    (swaps, sorted) <- bubbleSort (listArray (0, n-1) a)

    putStrLn $ "Array is sorted in " ++ show swaps ++ " swaps."
    putStrLn $ "First Element: " ++ show (sorted ! 0)
    putStrLn $ "Last Element: " ++ show (sorted ! (n - 1))
