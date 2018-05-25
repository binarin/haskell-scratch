{-# LANGUAGE StandaloneDeriving #-}
module LeftistHeap where

import Test.QuickCheck
import Data.List

data Heap a where
  Empty :: Heap a
  Heap :: Int -> a -> Heap a -> Heap a -> Heap a

deriving instance Show a => Show (Heap a)

singleton :: a -> Heap a
singleton a = Heap 1 a Empty Empty

top :: Heap a -> Maybe a
top Empty = Nothing
top (Heap _ a _ _) = Just a

pop :: Ord a => Heap a -> Maybe (a, Heap a)
pop Empty = Nothing
pop (Heap _ a l r) = Just (a, merge l r)

fromList :: (Ord a, Show a) => [a] -> Heap a
fromList as = head $ pairwiseMerge (singleton <$> as)
  where
    pairwiseMerge [] = []
    pairwiseMerge (h:[]) = [h]
    pairwiseMerge (h1:h2:hs) = pairwiseMerge (merge h1 h2 : pairwiseMerge hs)

merge :: Ord a => Heap a -> Heap a -> Heap a
merge Empty h = h
merge h Empty = h
merge h1@(Heap _ a1 l1 r1) h2@(Heap _ a2 l2 r2)
  | a1 < a2 = mkHeap a1 l1 (merge r1 h2)
  | otherwise = mkHeap a2 l2 (merge h1 r2)
  where
    rank Empty = 0
    rank (Heap rnk _ _ _) = rnk
    mkHeap a b1 b2 =
      let rank1 = rank b1
          rank2 = rank b2
      in if rank1 < rank2
      then Heap (1 + rank1) a b2 b1
      else Heap (1 + rank2) a b1 b2

prop_smallestOnTop :: NonEmptyList Int -> Bool
prop_smallestOnTop (NonEmpty lst) =
  let
    heap = fromList lst
    expectedMin = minimum lst
  in
    top heap == Just expectedMin

prop_popsInProperOrder :: NonEmptyList Int -> Bool
prop_popsInProperOrder (NonEmpty lst) =
  sort lst == popTillEmpty (fromList lst)
  where
    popTillEmpty h = case pop h of
      Nothing -> []
      Just (a, h') -> a : popTillEmpty h'
