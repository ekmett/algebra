{-# LANGUAGE CPP, NoImplicitPrelude, FlexibleInstances, UndecidableInstances, DefaultSignatures #-}
module Numeric.Domain.Internal where

import Data.Maybe(fromJust)
import Numeric.Additive.Group
import Numeric.Algebra.Class
import Numeric.Algebra.Commutative
import Numeric.Algebra.Division
import Numeric.Natural (Natural)
import Numeric.Semiring.ZeroProduct
import Numeric.Algebra.Unital.UnitNormalForm
import Numeric.Ring.Class
import Numeric.Decidable.Zero
import Numeric.Decidable.Units

import Prelude (Integer, Maybe (..), Bool(..),
                otherwise, fst, snd, ($), (.))
import qualified Prelude                 as P

infixl 7 `quot`, `rem`
infix  7 `divide`, `divides`, `maybeQuot`

-- | (Integral) domain is the integral semiring.
class (ZeroProductSemiring d, Ring d) => Domain d
instance (ZeroProductSemiring d, Ring d) => Domain d

-- | An integral domain is a commutative domain in which 1≠0.
class (Domain d, Commutative d) => IntegralDomain d where
    divides :: d -> d -> Bool
    default divides :: (Euclidean d) => d -> d -> Bool
    m `divides` n 
        | isZero m = False
        | otherwise = isZero (n `rem` m)
    maybeQuot :: d -> d -> Maybe d
    default maybeQuot :: (Euclidean d) => d -> d -> Maybe d
    m `maybeQuot` n
        | isZero n = Nothing
        | otherwise = let (q,r) = m `divide` n in
                      if isZero r then Just q else Nothing

instance IntegralDomain Integer

class (IntegralDomain d, UnitNormalForm d, DecidableZero d) => GCDDomain d where
    gcd :: d -> d -> d
    default gcd :: (PID d) => d -> d -> d
    gcd a b = let (r,_,_) = egcd a b in r
    {-# INLINE gcd #-}

    reduceFraction :: d -> d -> (d,d)
    reduceFraction a b =
        let c = gcd a b in
        (fromJust (a `maybeQuot` c), fromJust (b `maybeQuot` c))

    lcm :: d -> d -> d
    lcm p q = fromJust $ (p * q) `maybeQuot` (gcd p q)

instance GCDDomain Integer

class (GCDDomain d) => UFD d

instance UFD Integer

class (UFD d) => PID d where
    egcd :: d -> d -> (d,d,d)
    default egcd :: (Euclidean d) => d -> d -> (d,d,d)
    egcd a b = P.head (euclid a b)
    {-# INLINE egcd #-}

instance PID Integer

class (PID d) => Euclidean d where
  -- | Euclidean (degree) function on @r@.
  degree :: d -> Maybe Natural
  -- | Division algorithm. @a `divide` b@ calculates
  --   quotient and reminder of @a@ divided by @b@.
  --
  -- prop> let (q, r) = divide a p in p*q + r == a && degree r < degree q
  divide :: d                   -- ^ elements divided by
         -> d                   -- ^ divisor
         -> (d,d)               -- ^ quotient and remin
  default divide :: (Division d) => d -> d -> (d,d)
  -- Be strict in order to make sure division by zero gets caught
  divide a b = let q = a/b in (q,P.seq q zero)

  quot :: d -> d -> d
  quot a b = fst $ a `divide` b
  {-# INLINE quot #-}

  rem :: d -> d -> d
  rem a b = snd $ a `divide` b
  {-# INLINE rem #-}

#if (__GLASGOW_HASKELL__ > 708)
  {-# MINIMAL splitUnit, degree, divide #-}
#endif

instance Euclidean Integer where
  degree = Just . P.fromInteger . P.abs
  {-# INLINE degree #-}

  divide = P.divMod
  {-# INLINE divide #-}


-- | Extended euclidean algorithm.
--
-- prop> euclid f g == xs ==> all (\(r, s, t) -> r == f * s + g * t) xs
euclid :: (Euclidean d) =>  d -> d -> [(d,d,d)]
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
