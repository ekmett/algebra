{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- This package is an unfortunate ball of mud forced on me by mutual dependencies
module Numeric.Semiring.Internal
  ( 
  -- * Multiplicative Semigroups
    Multiplicative(..)
  , pow1pIntegral
  , product1
  -- * Semirings
  , Semiring
  -- * Associative algebras of free semigroups over semirings
  , Algebra(..)
  ) where

import Data.Foldable hiding (sum, concat)
import Data.Semigroup.Foldable
import Data.Int
import Data.Sequence hiding (reverse)
import qualified Data.Sequence as Seq
import Data.Word
import Prelude hiding ((*), (+), negate, subtract,(-), recip, (/), foldr, sum, product, replicate, concat)
import qualified Prelude
import Numeric.Natural.Internal
import Numeric.Additive
import Numeric.Addition.Abelian

infixr 8 `pow1p`
infixl 7 *

-- | A multiplicative semigroup
class Multiplicative r where
  (*) :: r -> r -> r 

-- class Multiplicative r => PowerAssociative r where
  -- pow1p x n = pow x (1 + n)
  pow1p :: Whole n => r -> n -> r
  pow1p x0 y0 = f x0 (y0 Prelude.+ 1) where
    f x y 
      | even y = f (x * x) (y `quot` 2)
      | y == 1 = x
      | otherwise = g (x * x) ((y Prelude.- 1) `quot` 2) x
    g x y z 
      | even y = g (x * x) (y `quot` 2) z
      | y == 1 = x * z
      | otherwise = g (x * x) ((y Prelude.- 1) `quot` 2) (x * z)

-- class PowerAssociative r => Assocative r where
  productWith1 :: Foldable1 f => (a -> r) -> f a -> r
  productWith1 f = maybe (error "Numeric.Multiplicative.Semigroup.productWith1: empty structure") id . foldl' mf Nothing
    where 
      mf Nothing y = Just $! f y
      mf (Just x) y = Just $! x * f y

product1 :: (Foldable1 f, Multiplicative r) => f r -> r
product1 = productWith1 id

pow1pIntegral :: (Integral r, Integral n) => r -> n -> r
pow1pIntegral r n = r ^ (1 Prelude.+ n)

instance Multiplicative Bool where
  (*) = (&&)
  pow1p m _ = m

instance Multiplicative Natural where
  (*) = (Prelude.*)
  pow1p = pow1pIntegral

instance Multiplicative Integer where
  (*) = (Prelude.*)
  pow1p = pow1pIntegral

instance Multiplicative Int where
  (*) = (Prelude.*)
  pow1p = pow1pIntegral

instance Multiplicative Int8 where
  (*) = (Prelude.*)
  pow1p = pow1pIntegral

instance Multiplicative Int16 where
  (*) = (Prelude.*)
  pow1p = pow1pIntegral

instance Multiplicative Int32 where
  (*) = (Prelude.*)
  pow1p = pow1pIntegral

instance Multiplicative Int64 where
  (*) = (Prelude.*)
  pow1p = pow1pIntegral

instance Multiplicative Word where
  (*) = (Prelude.*)
  pow1p = pow1pIntegral

instance Multiplicative Word8 where
  (*) = (Prelude.*)
  pow1p = pow1pIntegral

instance Multiplicative Word16 where
  (*) = (Prelude.*)
  pow1p = pow1pIntegral

instance Multiplicative Word32 where
  (*) = (Prelude.*)
  pow1p = pow1pIntegral

instance Multiplicative Word64 where
  (*) = (Prelude.*)
  pow1p = pow1pIntegral

instance Multiplicative () where
  _ * _ = ()
  pow1p _ _ = ()

instance (Multiplicative a, Multiplicative b) => Multiplicative (a,b) where
  (a,b) * (c,d) = (a * c, b * d)

instance (Multiplicative a, Multiplicative b, Multiplicative c) => Multiplicative (a,b,c) where
  (a,b,c) * (i,j,k) = (a * i, b * j, c * k)

instance (Multiplicative a, Multiplicative b, Multiplicative c, Multiplicative d) => Multiplicative (a,b,c,d) where
  (a,b,c,d) * (i,j,k,l) = (a * i, b * j, c * k, d * l)

instance (Multiplicative a, Multiplicative b, Multiplicative c, Multiplicative d, Multiplicative e) => Multiplicative (a,b,c,d,e) where
  (a,b,c,d,e) * (i,j,k,l,m) = (a * i, b * j, c * k, d * l, e * m)

-- | A pair of an additive abelian semigroup, and a multiplicative semigroup, with the distributive laws:
-- 
-- > a(b + c) = ab + ac
-- > (a + b)c = ac + bc
--
-- Common notation includes the laws for additive and multiplicative identity in semiring.
--
-- If you want that, look at 'Rig' instead.
--
-- Ideally we'd use the cyclic definition:
--
-- > class (LeftModule r r, RightModule r r, Additive r, Abelian r, Multiplicative r) => Semiring r
--
-- to enforce that every semiring r is an r-module over itself, but Haskell doesn't like that.
class (Additive r, Abelian r, Multiplicative r) => Semiring r

instance Semiring Integer
instance Semiring Natural
instance Semiring Bool
instance Semiring Int
instance Semiring Int8
instance Semiring Int16
instance Semiring Int32
instance Semiring Int64
instance Semiring Word
instance Semiring Word8
instance Semiring Word16
instance Semiring Word32
instance Semiring Word64
instance Semiring ()
instance (Semiring a, Semiring b) => Semiring (a, b)
instance (Semiring a, Semiring b, Semiring c) => Semiring (a, b, c)
instance (Semiring a, Semiring b, Semiring c, Semiring d) => Semiring (a, b, c, d)
instance (Semiring a, Semiring b, Semiring c, Semiring d, Semiring e) => Semiring (a, b, c, d, e)

-- | An associative algebra built with a free module over a semiring
class Semiring r => Algebra r a where
  mult :: (a -> a -> r) -> a -> r

instance Algebra r a => Multiplicative (a -> r) where
  f * g = mult $ \a b -> f a * g b

instance Algebra r a => Semiring (a -> r) 

  
instance Algebra () a where
  mult _ _ = ()

-- | The tensor algebra
instance Semiring r => Algebra r [a] where
  mult f = go [] where
    go ls rrs@(r:rs) = f (reverse ls) rrs + go (r:ls) rs
    go ls [] = f (reverse ls) []

-- | The tensor algebra
instance Semiring r => Algebra r (Seq a) where
  mult f = go Seq.empty where
    go ls s = case viewl s of
       EmptyL -> f ls s 
       r :< rs -> f ls s + go (ls |> r) rs

instance (Algebra r a, Algebra r b) => Algebra r (a,b) where
  mult f (a,b) = mult (\a1 a2 -> mult (\b1 b2 -> f (a1,b1) (a2,b2)) b) a

instance (Algebra r a, Algebra r b, Algebra r c) => Algebra r (a,b,c) where
  mult f (a,b,c) = mult (\a1 a2 -> mult (\b1 b2 -> mult (\c1 c2 -> f (a1,b1,c1) (a2,b2,c2)) c) b) a

instance (Algebra r a, Algebra r b, Algebra r c, Algebra r d) => Algebra r (a,b,c,d) where
  mult f (a,b,c,d) = mult (\a1 a2 -> mult (\b1 b2 -> mult (\c1 c2 -> mult (\d1 d2 -> f (a1,b1,c1,d1) (a2,b2,c2,d2)) d) c) b) a

instance (Algebra r a, Algebra r b, Algebra r c, Algebra r d, Algebra r e) => Algebra r (a,b,c,d,e) where
  mult f (a,b,c,d,e) = mult (\a1 a2 -> mult (\b1 b2 -> mult (\c1 c2 -> mult (\d1 d2 -> mult (\e1 e2 -> f (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2)) e) d) c) b) a

-- TODO: check this
instance (Algebra r b, Algebra r a) => Algebra (b -> r) a where
  mult f a b = mult (\a1 a2 -> f a1 a2 b) a
