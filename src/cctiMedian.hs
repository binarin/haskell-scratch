{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Data.Maybe
import Control.Monad

data MinHeap a = MinEmpty | MinHeap Int a (MinHeap a) (MinHeap a) deriving (Show)
data MaxHeap a = MaxEmpty | MaxHeap Int a (MaxHeap a) (MaxHeap a) deriving (Show)

data Median = Median { _minHeap :: MinHeap Int
                     , _minSize :: Int
                     , _maxHeap :: MaxHeap Int
                     , _maxSize :: Int
                     } deriving (Show)

emptyMedian :: Median
emptyMedian = Median MinEmpty 0 MaxEmpty 0

minTop :: Ord a => MinHeap a -> Maybe a
minTop MinEmpty = Nothing
minTop (MinHeap _ a _ _) = Just a

maxTop :: Ord a => MaxHeap a -> Maybe a
maxTop MaxEmpty = Nothing
maxTop (MaxHeap _ a _ _) = Just a

median :: Median -> Maybe Double
median Median { _minSize = 0, _maxSize = 0 } = Nothing
median Median { .. }
  | (_minSize + _maxSize) `mod` 2 == 0 = Just $ (minTopValue + maxTopValue) / 2.0
  | _minSize > _maxSize = Just minTopValue
  | otherwise = Just maxTopValue
  where
    toDouble x = maybe 0 fromIntegral x
    minTopValue = toDouble $ minTop _minHeap
    maxTopValue = toDouble $ maxTop _maxHeap

add :: Int -> Median -> Median
add a m@Median {..} =
  balance1 $ if fromIntegral a > fromMaybe 0 (median m)
             then m { _minSize = _minSize + 1, _minHeap = minMerge _minHeap (minSingleton a) }
             else m { _maxSize = _maxSize + 1, _maxHeap = maxMerge _maxHeap (maxSingleton a) }
  where
    balance1 m'@Median {..}
      | _maxSize > _minSize + 1 =
        case maxPop _maxHeap of
          Just (elt, _maxHeap') -> m { _minSize = _minSize + 1
                                     , _minHeap = minMerge _minHeap (minSingleton elt)
                                     , _maxSize = _maxSize - 1
                                     , _maxHeap = _maxHeap'
                                     }
          Nothing ->
            error "cant be"
      | _minSize > _maxSize + 1 =
        case minPop _minHeap of
          Just (elt, _minHeap') -> m { _minSize = _minSize - 1
                                     , _minHeap = _minHeap'
                                     , _maxSize = _maxSize + 1
                                     , _maxHeap = maxMerge _maxHeap (maxSingleton elt)
                                     }
          Nothing ->
            error "cant be"
      | otherwise = m'

minSingleton :: a -> MinHeap a
minSingleton a = MinHeap 1 a MinEmpty MinEmpty

minMerge :: Ord a => MinHeap a -> MinHeap a -> MinHeap a
minMerge MinEmpty h = h
minMerge h MinEmpty = h
minMerge h1@(MinHeap _ a1 l1 r1) h2@(MinHeap _ a2 l2 r2)
  | a1 < a2 = mkMinHeap a1 l1 (minMerge r1 h2)
  | otherwise = mkMinHeap a2 l2 (minMerge h1 r2)
  where
    rank MinEmpty = 0
    rank (MinHeap rnk _ _ _) = rnk
    mkMinHeap a b1 b2 =
      let rank1 = rank b1
          rank2 = rank b2
      in if rank1 < rank2
      then MinHeap (1 + rank1) a b2 b1
      else MinHeap (1 + rank2) a b1 b2

maxSingleton :: a -> MaxHeap a
maxSingleton a = MaxHeap 1 a MaxEmpty MaxEmpty

maxMerge :: Ord a => MaxHeap a -> MaxHeap a -> MaxHeap a
maxMerge MaxEmpty h = h
maxMerge h MaxEmpty = h
maxMerge h1@(MaxHeap _ a1 l1 r1) h2@(MaxHeap _ a2 l2 r2)
  | a1 > a2 = mkMaxHeap a1 l1 (maxMerge r1 h2)
  | otherwise = mkMaxHeap a2 l2 (maxMerge h1 r2)
  where
    rank MaxEmpty = 0
    rank (MaxHeap rnk _ _ _) = rnk
    mkMaxHeap a b1 b2 =
      let rank1 = rank b1
          rank2 = rank b2
      in if rank1 < rank2
      then MaxHeap (1 + rank1) a b2 b1
      else MaxHeap (1 + rank2) a b1 b2

minPop :: Ord a => MinHeap a -> Maybe (a, MinHeap a)
minPop MinEmpty = Nothing
minPop (MinHeap _ a l r) = Just (a, minMerge l r)

maxPop :: Ord a => MaxHeap a -> Maybe (a, MaxHeap a)
maxPop MaxEmpty = Nothing
maxPop (MaxHeap _ a l r) = Just (a, maxMerge l r)

readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO()
main = do
    n <- readLn :: IO Int

    aTemp :: [Int] <- fmap read <$> readMultipleLinesAsStringArray n

    let
      addAndPrint med a = do
        let med' = add a med
        putStrLn $ show $ fromMaybe 0 $ median med'
        pure med'

    foldM addAndPrint emptyMedian aTemp

    pure ()
