{-

https://www.hackerrank.com/challenges/ctci-balanced-brackets/problem

-}

{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Bits
import Data.List
import Data.Set
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

isBalanced :: String -> Bool
isBalanced s = go s ""
  where
    go [] [] = True
    go [] _ = False
    go ('[':ss) stack = go ss (']':stack)
    go ('{':ss) stack = go ss ('}':stack)
    go ('(':ss) stack = go ss (')':stack)
    go (']':ss) (']':stack) = go ss stack
    go (']':ss) _ = False
    go ('}':ss) ('}':stack) = go ss stack
    go ('}':ss) _ = False
    go (')':ss) (')':stack) = go ss stack
    go (')':ss) _ = False


showBool :: Bool -> String
showBool True = "YES"
showBool False = "NO"

main :: IO()
main = do
    t <- readLn :: IO Int

    forM_ [1..t] $ \t_itr -> do
        isBalanced <$> getLine >>= putStrLn . showBool
