module Numeric.Semigroup.Multiplicative
  ( Multiplicative(..)
  , pow1pIntegral
  , product1
  ) where

import Data.Foldable hiding (sum)
import Data.Int
import Data.Word
import Data.Semigroup.Foldable
-- import Numeric.Monoid.Additive
-- import Numeric.Addition.Partitionable
import Numeric.Natural.Internal
import qualified Prelude
import Prelude hiding ((*), (+), negate, (-), recip, (/), foldr, sum, product)

infixr 8 `pow1p`
infixl 7 *

-- | A multiplicative semigroup
class Multiplicative r where
  (*) :: r -> r -> r 

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

  productWith1 :: Foldable1 f => (a -> r) -> f a -> r
  productWith1 f = maybe (error "Numeric.Multiplicative.Semigroup.productWith1: empty structure") id . foldl' mf Nothing
    where 
      mf Nothing y = Just $! f y
      mf (Just x) y = Just $! x * f y

product1 :: (Foldable1 f, Multiplicative r) => f r -> r
product1 = productWith1 id

{-
instance (Partitionable a, AdditiveMonoid r, Multiplicative r) => Multiplicative (a -> r) where
  f * g = sum . partitionWith (\a b -> f a * g b)
  pow1p x0 y0 = f x0 (1 Prelude.+ y0)
    where
      f x y 
        | even y = f (x * x) (y `quot` 2)
        | y == 1 = x
        | otherwise = g (x * x) (unsafePred y `quot` 2) x
      g x y z 
        | even y = g (x * x) (y `quot` 2) z
        | y == 1 = x * z
        | otherwise = g (x * x) (unsafePred y `quot` 2) (x * z)
-}

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
