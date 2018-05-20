{-

https://www.hackerrank.com/challenges/ctci-making-anagrams/problem

-}
module Main where

import Data.Map as M hiding (foldr)
import Data.List

count :: [Char] -> M.Map Char Int
count s = foldr addIt M.empty s
  where
    addIt ch m = M.alter incIt ch m
    incIt Nothing = Just $ 1
    incIt (Just it) = Just $ it + 1

main :: IO ()
main = do
  a <- count <$> getLine
  b <- count <$> getLine

  let all = nub $ M.keys a ++ M.keys b
      howMuch :: Char -> Int -> Int
      howMuch ch cnt =
        let ac = M.findWithDefault 0 ch a
            bc = M.findWithDefault 0 ch b
        in
          cnt + (abs $ ac - bc)
  putStrLn $ show $ b
  putStrLn $ show $ foldr howMuch 0 all
