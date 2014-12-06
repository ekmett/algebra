{-# LANGUAGE CPP, NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module Numeric.Domain.Internal where

import Numeric.Additive.Group
import Numeric.Algebra.Class
import Numeric.Algebra.Unital
import Numeric.Natural (Natural)
import Numeric.Semiring.ZeroProduct
import Numeric.Ring.Class
import Numeric.Decidable.Zero
import Numeric.Decidable.Units

import Prelude (Eq (..), Integer, Maybe (..), abs)
import Prelude (fst, otherwise)
import Prelude (signum, snd, ($), (.))
import qualified Prelude                 as P

infixl 7 `quot`, `rem`
infix  7 `divide`

-- | (Integral) domain is the integral semiring.
class (ZeroProductSemiring d, Ring d) => Domain d
instance (ZeroProductSemiring d, Ring d) => Domain d

class (Ring r, DecidableZero r, DecidableUnits r, Domain r) => Euclidean r where
  -- | @splitUnit r@ calculates its leading unit and normal form.
  --
  -- prop> let (u, n) = splitUnit r in r == u * n && fst (splitUnit n) == one && isUnit u
  splitUnit :: r -> (r, r)
  -- | Euclidean (degree) function on @r@.
  degree :: r -> Maybe Natural
  -- | Division algorithm. @a `divide` b@ calculates
  --   quotient and reminder of @a@ divided by @b@.
  --
  -- prop> let (q, r) = divide a p in p*q + r == a && degree r < degree q
  divide :: r                   -- ^ elements divided by
         -> r                   -- ^ divisor
         -> (r,r)               -- ^ quotient and remin
  quot :: r -> r -> r
  quot a b = fst $ a `divide` b
  {-# INLINE quot #-}

  rem :: r -> r -> r
  rem a b = snd $ a `divide` b
  {-# INLINE rem #-}

  -- | @'gcd' a b@ calculates greatest common divisor of @a@ and @b@.
  gcd :: r -> r -> r
  gcd a b = let (g,_,_):_ = euclid a b
            in g
  {-# INLINE gcd #-}

  -- | Extended euclidean algorithm.
  --
  -- prop> euclid f g == xs ==> all (\(r, s, t) -> r == f * s + g * t) xs
  euclid :: r -> r -> [(r,r,r)]
  euclid f g =
    let (ug, g') = splitUnit g
        Just t'  = recipUnit ug
        (uf, f') = splitUnit f
        Just s   = recipUnit uf
    in step [(g', zero, t'), (f', s, zero)]
    where
      step acc@((r',s',t'):(r,s,t):_)
        | isZero r' = P.tail acc
        | otherwise =
          let q         = r `quot` r'
              (ur, r'') = splitUnit $ r - q * r'
              Just u    = recipUnit ur
              s''       = (s - q * s') * u
              t''       = (t - q * t') * u
          in step ((r'', s'', t'') : acc)
      step _ = P.error "cannot happen!"
#if (__GLASGOW_HASKELL__ > 708)
  {-# MINIMAL splitUnit, degree, divide #-}
#endif

instance Euclidean Integer where
  splitUnit 0 = (1, 0)
  splitUnit n = (signum n, abs n)
  {-# INLINE splitUnit #-}

  degree = Just . P.fromInteger . abs
  {-# INLINE degree #-}

  divide = P.divMod
  {-# INLINE divide #-}

