module Numeric.Multiplicative.Class
  ( Multiplicative(..)
  , powSemigroup
  , powIntegral
  , product1
  ) where

import Data.Foldable
import Data.Int
import Data.Word
import Data.Semigroup.Foldable
import qualified Prelude
import Prelude hiding ((*), (+), negate, (-), recip, (/), foldr, sum, product)

infixr 8 ^
infixl 7 *

-- | A multiplicative semigroup
class Multiplicative r where
  (*) :: r -> r -> r 
  (^) :: Integral n => r -> n -> r
  productWith1 :: Foldable1 f => (a -> r) -> f a -> r
  productWith1 f = maybe (error "Numeric.Multiplicative.Semigroup.productWith1: empty structure") id . foldl' mf Nothing
     where mf Nothing y = Just $! f y
           mf (Just x) y = Just $! x * f y

product1 :: (Foldable1 f, Multiplicative r) => f r -> r
product1 = productWith1 id

powSemigroup :: (Multiplicative r, Integral n) => r -> n -> r
powSemigroup x0 y0 = case compare y0 0 of
    LT -> error "powSemigroup: negative length"
    EQ -> error "powSemigroup: zero length"
    GT -> f x0 y0
    where
        f x y 
            | even y = f (x * x) (y `quot` 2)
            | y == 1 = x
            | otherwise = g (x * x) ((y Prelude.- 1) `quot` 2) x
        g x y z 
            | even y = g (x * x) (y `quot` 2) z
            | y == 1 = x * z
            | otherwise = g (x * x) ((y Prelude.- 1) `quot` 2) (x * z)

-- a suitable default definition for (^) for instances of the Prelude Integral class,
-- addressing possible negative exponentiation of units 1 and -1.
powIntegral :: (Integral r, Integral n) => r -> n -> r
powIntegral (-1) y0 
  | even y0 = 1
  | otherwise = -1
powIntegral 1 _ = 1
powIntegral x0 y0 = case compare y0 0 of
    LT -> error "negative non-unit recipriocal"
    EQ -> 1
    GT -> f x0 y0
    where
        f x y
            | even y = f (x Prelude.* x) (y `quot` 2)
            | y == 1 = x
            | otherwise = g (x Prelude.* x) ((y Prelude.- 1) `quot` 2) x
        g x y z
            | even y = g (x Prelude.* x) (y `quot` 2) z
            | y == 1 = x Prelude.* z
            | otherwise = g (x Prelude.* x) ((y Prelude.- 1) `quot` 2) (x Prelude.* z)

{-
-- requires flexible instances
instance Multiplicative (r -> r) where
  (*) = (.)
  x0 ^ y0 = case compare y0 0 of
    LT -> error "(a -> a).(^) : negative length"
    EQ -> id
    GT -> f x0 y0
    where
        f x y 
            | even y = f (x * x) (y `quot` 2)
            | y == 1 = x
            | otherwise = g (x * x) ((y Prelude.- 1) `quot` 2) x
        g x y z 
            | even y = g (x * x) (y `quot` 2) z
            | y == 1 = x * z
            | otherwise = g (x * x) ((y Prelude.- 1) `quot` 2) (x * z)
-}

instance Multiplicative Integer where
  (*) = (Prelude.*)
  (^) = powIntegral
instance Multiplicative Int where
  (*) = (Prelude.*)
  (^) = powIntegral
instance Multiplicative Int8 where
  (*) = (Prelude.*)
  (^) = powIntegral
instance Multiplicative Int16 where
  (*) = (Prelude.*)
  (^) = powIntegral
instance Multiplicative Int32 where
  (*) = (Prelude.*)
  (^) = powIntegral
instance Multiplicative Int64 where
  (*) = (Prelude.*)
  (^) = powIntegral
instance Multiplicative Word where
  (*) = (Prelude.*)
  (^) = powIntegral
instance Multiplicative Word8 where
  (*) = (Prelude.*)
  (^) = powIntegral
instance Multiplicative Word16 where
  (*) = (Prelude.*)
  (^) = powIntegral
instance Multiplicative Word32 where
  (*) = (Prelude.*)
  (^) = powIntegral
instance Multiplicative Word64 where
  (*) = (Prelude.*)
  (^) = powIntegral
