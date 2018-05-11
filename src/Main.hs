{-

Walkthrough and excercises for:
http://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html

-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Main where
import Data.Aeson hiding (Bool)
import qualified Data.Vector as V
import GHC.TypeLits hiding (Nat)
import Data.Proxy
import Data.HashMap.Strict as H
import qualified Data.Text as T
import Data.Monoid
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8

data Nat = Zero | Succ Nat

data IntBool a where
  Int :: Int -> IntBool Int
  Bool :: Bool -> IntBool Bool

extractIntBool :: IntBool a -> a
extractIntBool (Int _) = 0
extractIntBool (Bool b) = b

data Vector (n :: Nat) a where
  VNil :: Vector 'Zero a
  VCons :: a -> Vector n a -> Vector ('Succ n) a

instance Show a => Show (Vector n a) where
  show VNil = "VNil"
  show (VCons a as) = "VCons " ++ show a ++ " (" ++ show as ++ ")"

type family Add n m where
  Add 'Zero n = n
  Add ('Succ n) m = 'Succ (Add n m)

append :: Vector n a -> Vector m a -> Vector (Add n m) a
append VNil rest = rest
append (VCons a rest) xs = VCons a (append rest xs)

data HList xs where
  HNil :: HList '[]
  (:::) :: a -> HList as -> HList (a ': as) -- ')

infixr 6 :::

instance Show (HList '[]) where
  show HNil = "HNil"

instance (Show (HList as), Show a) => Show (HList (a ': as)) -- where '))
  where
    show (a ::: rest) = show a ++ " ::: " ++ show rest

instance ToJSON (HList '[]) where
  toJSON HNil = Array $ V.fromList []

instance (ToJSON (HList as), ToJSON a) => ToJSON (HList (a : as)) where -- a ': as, but syntax highlighting
  toJSON (a ::: rest) = Array $ toJSON a `V.cons` restJSON
    where
      restJSON :: V.Vector Value = case toJSON rest of
        Array aJSON -> aJSON
        _ -> V.fromList []

newtype s >> a = Named a

data HRec xs where
  HEmpty :: HRec '[]
  HCons :: (s >> a) -> HRec xs -> HRec (s >> a : xs)

instance Show (HRec '[]) where
  show HEmpty = "HEmpty"

instance (Show a, KnownSymbol s, Show (HRec xs)) => Show (HRec (s >> a : xs)) where
  show (HCons (Named a) rest) =
    let val = show a
        key = symbolVal (Proxy :: Proxy s)
        more = show rest
     in "(" ++ key ++ ": " ++ val ++ ") " ++ more

instance ToJSON (HRec '[]) where
  toJSON _ = object []

instance (KnownSymbol s, ToJSON a, ToJSON (HRec xs)) =>  ToJSON (HRec (s >> a: xs)) where
  toJSON (HCons (Named a) rest) = Object $ aJSON <> restJSON
    where
      key = symbolVal (Proxy :: Proxy s)
      aJSON = H.singleton (T.pack key) (toJSON a)
      restJSON = case toJSON rest of
        Object kvs -> kvs
        _ -> mempty

instance FromJSON (HRec '[]) where
  parseJSON = withObject "object" $ \obj ->
    pure HEmpty

instance (KnownSymbol s, FromJSON a, FromJSON (HRec xs)) =>  FromJSON (HRec (s >> a : xs)) where
  parseJSON = withObject "object expected" $ \obj ->
    let key = symbolVal (Proxy :: Proxy s)
     in HCons <$> (Named <$> obj .: (T.pack key)) <*> parseJSON (Object obj)

main :: IO ()
main = do
  putStrLn "hello world"
