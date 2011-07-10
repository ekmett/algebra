module Numeric.Semigroup.Multiplicative.Internal
  ( 
  -- * Multiplicative Semigroups
    Multiplicative(..)
  , pow1pIntegral
  , product1
  -- * Factorable Multiplicative Semigroups
  , Factorable(..)
  -- * Semirings
  , Semiring
  ) where

import Data.Foldable hiding (sum, concat)
import Data.Int
import Data.Semigroup.Foldable
import Data.Word
import Data.List.NonEmpty
import Numeric.Natural.Internal
import Numeric.Semigroup.Additive
import Numeric.Addition.Abelian
import Prelude hiding ((*), (+), negate, (-), recip, (/), foldr, sum, product, replicate, concat)
import qualified Prelude

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

-- | `factorWith f c` returns a non-empty list containing `f a b` for all `a, b` such that `a * b = c`.
--
-- Results of factorWith f 0 are undefined and may result in either an error or an infinite list.

class Multiplicative m => Factorable m where
  factorWith :: (m -> m -> r) -> m -> NonEmpty r

instance Factorable Bool where
  factorWith f False = f False False :| [f False True, f True False]
  factorWith f True  = f True True :| []

instance Factorable () where
  factorWith f () = f () () :| []

concat :: NonEmpty (NonEmpty a) -> NonEmpty a
concat m = m >>= id

instance (Factorable a, Factorable b) => Factorable (a,b) where
  factorWith f (a,b) = concat $ factorWith (\ax ay -> 
                                factorWith (\bx by -> f (ax,bx) (ay,by)) b) a

instance (Factorable a, Factorable b, Factorable c) => Factorable (a,b,c) where
  factorWith f (a,b,c) = concat $ factorWith (\ax ay -> 
                            concat $ factorWith (\bx by -> 
                                     factorWith (\cx cy -> f (ax,bx,cx) (ay,by,cy)) c) b) a

instance (Factorable a, Factorable b, Factorable c,Factorable d ) => Factorable (a,b,c,d) where
  factorWith f (a,b,c,d) = concat $ factorWith (\ax ay -> 
                           concat $ factorWith (\bx by -> 
                           concat $ factorWith (\cx cy -> 
                                    factorWith (\dx dy -> f (ax,bx,cx,dx) (ay,by,cy,dy)) d) c) b) a

instance (Factorable a, Factorable b, Factorable c,Factorable d, Factorable e) => Factorable (a,b,c,d,e) where
  factorWith f (a,b,c,d,e) = concat $ factorWith (\ax ay -> 
                             concat $ factorWith (\bx by -> 
                             concat $ factorWith (\cx cy -> 
                             concat $ factorWith (\dx dy -> 
                                      factorWith (\ex ey -> f (ax,bx,cx,dx,ex) (ay,by,cy,dy,ey)) e) d) c) b) a

-- | A pair of an additive abelian semigroup, and a multiplicative semigroup, with the distributive laws:
-- 
-- > a(b + c) = ab + ac
-- > (a + b)c = ac + bc
--
-- Common notation includes the laws for additive and multiplicative identity in semiring.
--
-- We call that a 'Rig' instead, because it makes the connection between Semiring and Ring
-- analogous to Semigroup and Group, and with the sheer number of classes we're tossing around
-- we can use all the mnemonic devices we can get!

class (Additive r, Abelian r, Multiplicative r) => Semiring r

-- | Much needed to be moved around to keep this instance from being an orphan!
instance (Factorable a, Semiring r) => Multiplicative (a -> r) where
  f * g = sum1 . factorWith (\a b -> f a * g b)
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

instance (Factorable m, Semiring r) => Semiring (m -> r)
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

-- direct product of semirings
instance (Semiring a, Semiring b) => Semiring (a, b)
instance (Semiring a, Semiring b, Semiring c) => Semiring (a, b, c)
instance (Semiring a, Semiring b, Semiring c, Semiring d) => Semiring (a, b, c, d)
instance (Semiring a, Semiring b, Semiring c, Semiring d, Semiring e) => Semiring (a, b, c, d, e)
