{-

https://www.hackerrank.com/challenges/ctci-contacts/problem

-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Map as M

data Lookup = L { lInner :: M.Map Char Lookup
                , lCount :: Int
                }
  deriving (Show)

lInsert :: [Char] -> Lookup -> Lookup
lInsert [] (L i c) = L i (c + 1)
lInsert (c:cs) (L i ctr) = L i' (ctr + 1)
  where
    i' = M.insert c nested' i
    nested' = lInsert cs nested
    nested = case M.lookup c i of
      Nothing -> L M.empty 0
      Just it -> it

count :: [Char] -> Lookup -> Int
count [] (L _ c) = c
count (c:cs) (L i _) = case M.lookup c i of
  Nothing -> 0
  Just it -> count cs it

main :: IO ()
main = do
  n <- readLn :: IO Int
  go n (L M.empty 0)
 where
 go 0 _ = pure ()
 go n l = do
   (op:contact:_) <- words <$> getLine
   case op of
     "add" -> go (n - 1) (lInsert contact l)
     "find" -> do
       putStrLn $ show $ count contact l
       go (n - 1) l
     _ -> error "bad op"
