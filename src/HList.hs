{-# LANGUAGE GADTs, TypeOperators, DataKinds, PolyKinds, PartialTypeSignatures #-}
module HList where

data HList tys where
  Nil :: HList '[]
  (:>) :: h -> HList t -> HList (h : t)

infixr 5 :>

data Elem list elt where
  EZ :: Elem (x:xs) x
  ES :: Elem xs x -> Elem (y:xs) x

get :: Elem tys ty -> HList tys -> ty
get EZ (x :> _) = x
get (ES es) (_ :> xs) = get es xs
