{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-

https://www.hackerrank.com/challenges/ctci-queue-using-two-stacks/problem

-}

module Main where

import GHC.Stack

data Queue a = Queue { _read :: [a]
                     , _write :: [a]
                     } deriving (Show)

mkQueue :: Queue a
mkQueue = Queue [] []

push :: a -> Queue a -> Queue a
push a q@Queue {..} = q { _write = a : _write }

pop :: Queue a -> Maybe (a, Queue a)
pop Queue {_read = [], _write = []} = Nothing
pop q@Queue {_read = [], _write} = pop $ q { _read = reverse _write, _write = [] }
pop q@Queue {_read = (x:xs) } = Just (x, q { _read = xs })

peek :: Queue a -> Maybe (a, Queue a)
peek Queue {_read = [], _write = []} = Nothing
peek q@Queue {_read = [], _write} = peek $ q { _read = reverse _write, _write = [] }
peek q@Queue {_read = (x:xs) } = Just (x, q)


main :: HasCallStack => IO ()
main = do
  numQueries :: Int <- readLn

  let go 0 _ = pure ()
      go n q = do
        query <- getLine
        case query of
          "2" -> do
            case pop q of
              Just (_, q') ->
                go (n - 1) q'
              Nothing ->
                error "Shouldn't happen"
          "3" ->
            case peek q of
              Just (elt, q') -> do
                putStrLn $ show elt
                go (n - 1) q'
              Nothing ->
                error "Shouldn't happen"
          '1':' ':rest -> do
            let num = read rest
            go (n - 1) (push num q)

  go numQueries (mkQueue :: Queue Int)
