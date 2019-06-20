{-# LANGUAGE Strict #-}
{-

https://www.hackerrank.com/challenges/ctci-merge-sort/problem

-}
module Main where

import Control.Monad
import Data.List
import System.IO
import System.Environment

mergeSort :: Ord a => [a] -> (Int, [a])
mergeSort [] = (0, [])
mergeSort (x:[]) = (0, [x])
mergeSort lst =
  let (left, right) = splitAt (length lst `div` 2) lst
      (leftLength, rightLength) = (length left, length right)
      (leftInvs, left') = mergeSort left
      (rightInvs, right') = mergeSort right
      (mergeInvs, sorted) = merge left' leftLength right' rightLength
  in (leftInvs + rightInvs + mergeInvs, sorted)

merge :: Ord a => [a] -> Int -> [a] -> Int -> (Int, [a])
merge [] _ bs _ = (0, bs)
merge as _ [] _ = (0, as)
merge as@(a:ar) al bs@(b:br) bl
  | a <= b =
      let (invs, merged) = merge ar (al - 1) bs bl
      in (invs, a:merged)
  | otherwise =
      let (invs, merged) = merge as al br (bl - 1)
      in (invs + al, b:merged)

main :: IO ()
main = do
  stdout <- getEnv "OUTPUT_PATH"
  fptr <- openFile stdout WriteMode

  t <- readLn :: IO Int

  forM_ [1..t] $ \t_itr -> do
      n <- readLn :: IO Int

      arrTemp <- getLine

      let arr = Data.List.map (read :: String -> Int) . words $ arrTemp

      let (result, _) = mergeSort arr

      hPutStrLn fptr $ show result

  hFlush fptr
  hClose fptr
