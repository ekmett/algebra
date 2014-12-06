{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude, RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables, ViewPatterns                          #-}
module Numeric.Field.Fraction
  ( Fraction
  , numerator
  , denominator
  , Ratio
  , (%)
  ) where
import Data.Proxy
import Numeric.Additive.Class
import Numeric.Additive.Group
import Numeric.Algebra.Class
import Numeric.Algebra.Commutative
import Numeric.Algebra.Division
import Numeric.Algebra.Unital
import Numeric.Algebra.Unital.UnitNormalForm
import Numeric.Decidable.Units
import Numeric.Decidable.Zero
import Numeric.Domain.GCD
import Numeric.Domain.Euclidean
import Numeric.Natural
import Numeric.Rig.Characteristic
import Numeric.Rig.Class
import Numeric.Ring.Class
import Numeric.Semiring.ZeroProduct
import Prelude                     hiding (Integral (..), Num (..), gcd, lcm)

-- | Fraction field @k(D)@ of 'Euclidean' domain @D@.
data Fraction d = Fraction !d !d

-- Invariants: r == Fraction p q
--         ==> leadingUnit q == one && q /= 0
--          && isUnit (gcd p q)

-- | Convenient synonym for 'Fraction'.
type Ratio = Fraction

instance (Eq d, Show d, Unital d) => Show (Fraction d) where
  showsPrec d (Fraction p q)
   | q == one    = showsPrec d p
   | otherwise = showParen (d > 5) $ showsPrec 6 p . showString " / " . showsPrec 6 q

infixl 7 %
(%) :: Euclidean d => d -> d -> Fraction d
a % b = let (ua, a') = splitUnit a
            (ub, b') = splitUnit b
            Just ub' = recipUnit ub
            r = gcd a' b'
        in Fraction (ua * ub' * a' `quot` r) (b' `quot` r)

numerator :: Fraction t -> t
numerator (Fraction q _) = q
{-# INLINE numerator #-}

denominator :: Fraction t -> t
denominator (Fraction _ p) = p
{-# INLINE denominator #-}

instance Euclidean d => ZeroProductSemiring (Fraction d)
instance (Eq d, Multiplicative d) => Eq (Fraction d) where
  Fraction p q == Fraction s t = p*t == q*s
  {-# INLINE (==) #-}

instance (Ord d, Multiplicative d) => Ord (Fraction d)  where
  compare (Fraction p q) (Fraction p' q') = compare (p*q') (p'*q)
  {-# INLINE compare #-}

instance Euclidean d => Division (Fraction d) where
  recip (Fraction p q) | isZero p  = error "Ratio has zero denominator!"
                       | otherwise = let (recipUnit -> Just u, p') = splitUnit p
                                     in Fraction (q * u) p'
  Fraction p q / Fraction s t = (p*t) % (q*s)
  {-# INLINE recip #-}
  {-# INLINE (/) #-}

instance (Commutative d, Euclidean d) => Commutative (Fraction d)

instance Euclidean d => DecidableZero (Fraction d) where
  isZero (Fraction p _) = isZero p
  {-# INLINE isZero #-}

instance Euclidean d => DecidableUnits (Fraction d) where
  isUnit (Fraction p _) = not $ isZero p
  {-# INLINE isUnit #-}
  recipUnit (Fraction p q) | isZero p  = Nothing
                           | otherwise = Just (Fraction q p)
  {-# INLINE recipUnit #-}
instance Euclidean d => Ring (Fraction d)
instance Euclidean d => Abelian (Fraction d)
instance Euclidean d => Semiring (Fraction d)
instance Euclidean d => Group (Fraction d) where
  negate (Fraction p q) = Fraction (negate p) q
  Fraction p q - Fraction p' q' = (p*q'-p'*q) % (q*q')
instance Euclidean d => Monoidal (Fraction d) where
  zero = Fraction zero one
  {-# INLINE zero #-}
instance Euclidean d => LeftModule Integer (Fraction d) where
  n .* Fraction p r = (n .* p) % r
  {-# INLINE (.*) #-}
instance Euclidean d => RightModule Integer (Fraction d) where
  Fraction p r *. n = (p *. n) % r
  {-# INLINE (*.) #-}
instance Euclidean d => LeftModule Natural (Fraction d) where
  n .* Fraction p r = (n .* p) % r
  {-# INLINE (.*) #-}
instance Euclidean d => RightModule Natural (Fraction d) where
  Fraction p r *. n = (p *. n) % r
  {-# INLINE (*.) #-}
instance Euclidean d => Additive (Fraction d) where
  Fraction p q + Fraction s t =
    let u = gcd q t
    in Fraction (p * t `quot` u + s*q`quot`u) (q*t`quot`u)
  {-# INLINE (+) #-}
instance Euclidean d => Unital (Fraction d) where
  one = Fraction one one
  {-# INLINE one #-}
instance Euclidean d => Multiplicative (Fraction d) where
  Fraction p q * Fraction s t = (p*s) % (q*t)
instance Euclidean d => Rig (Fraction d)

instance (Characteristic d, Euclidean d) => Characteristic (Fraction d) where
  char _ = char (Proxy :: Proxy d)
