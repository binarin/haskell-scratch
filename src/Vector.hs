{-# LANGUAGE TypeInType #-}
{-

  https://www.schoolofhaskell.com/user/konn/prove-your-haskell-for-great-safety/dependent-types-in-haskell

-}
{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, UndecidableInstances, GADTs, StandaloneDeriving #-}

module Vector where
import Prelude hiding (head, tail, map, init, last, zipWith)

data Nat = Z | S Nat

infixl 6 :+
infixl 7 :*

type family   (n :: Nat) :+ (m :: Nat) :: Nat
type instance 'Z     :+ m = m
type instance ('S n) :+ m = 'S (n :+ m)

type family   (n :: Nat) :* (m :: Nat) :: Nat
type instance 'Z     :* m = 'Z
type instance ('S n) :* m = n :* m :+ m

type family Min (n :: Nat) (m :: Nat) :: Nat
type instance Min 'Z m = 'Z
type instance Min n 'Z = 'Z
type instance Min ('S n) ('S m) = 'S (Min n m)

min :: Nat -> Nat -> Nat
min n m = go n m
  where go Z _ = m
        go _ Z = n
        go (S n') (S m') = go n' m'


data Vector a n where
  Nil :: Vector a 'Z
  (:-) :: a -> Vector a n -> Vector a ('S n)

infixr 5 :-

deriving instance Eq a => Eq (Vector a n)

toList :: Vector a n -> [a]
toList Nil = []
toList (x :- xs) = x : toList xs

data SNat a where
  SZ :: SNat 'Z
  SS :: SNat a -> SNat ('S a)

fromList :: SNat n -> [a] -> Maybe (Vector a n)
fromList SZ _  = Just Nil
fromList (SS _) [] = Nothing
fromList (SS p) (x:xs) = do
  rest <- fromList p xs
  pure $ x :- rest

map :: (a -> b) -> Vector a n -> Vector b n
map _ Nil = Nil
map f (x :- xs) = f x :- map f xs

uncons :: Vector a ('S n) -> (a, Vector a n)
uncons (x :- xs) = (x, xs)

init :: Vector a ('S n) -> Vector a n
init (x :- xs) = case xs of
  Nil -> Nil
  _ :- _ -> x :- init xs

last :: Vector a ('S n) -> a
last (x :- Nil) = x
last (_ :- xs@(_ :- _)) = last xs

instance Show a => Show (Vector a n) where
  showsPrec d = showsPrec d . toList

head :: Vector a ('S n) -> a
head (x :- _) = x

tail :: Vector a ('S n) -> Vector a n
tail (_ :- xs) = xs

append :: Vector a n -> Vector a m -> Vector a (n :+ m)
append (x :- xs) ys = x :- append xs ys
append Nil       ys = ys

zipWithSame :: (a -> b -> c) -> Vector a n -> Vector b n -> Vector c n
zipWithSame _ Nil Nil = Nil
zipWithSame f (a :- as) (b :- bs) = f a b :- zipWithSame f as bs

zipWith :: (a -> b -> c) -> Vector a n -> Vector b m -> Vector c (Min n m)
zipWith _ Nil _ = Nil
zipWith _ _ Nil = Nil
zipWith f (a :- as) (b :- bs) = f a b :- zipWith f as bs

main :: IO ()
main = do
  print $ head (1 :- 2 :- Nil)
  print $ tail (1 :- 2 :- Nil)
  -- | Uncommenting the line below causes type error
  -- print $ head Nil
