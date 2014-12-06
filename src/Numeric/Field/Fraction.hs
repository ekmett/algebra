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
import Numeric.Natural
import Numeric.Rig.Characteristic
import Numeric.Rig.Class
import Numeric.Ring.Class
import Numeric.Semiring.ZeroProduct
import Prelude                     hiding (Integral (..), Num (..), gcd, lcm)

-- | Fraction field @k(D)@ of 'GCDDomain' domain @D@.
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
(%) :: (GCDDomain d, DecidableUnits d, UnitNormalForm d) => d -> d -> Fraction d
a % b = let (ua, a') = splitUnit a
            (ub, b') = splitUnit b
            Just ub' = recipUnit ub
            (a'',b'') = reduceFraction a' b'
        in Fraction (ua * ub' * a'') (b'')

numerator :: Fraction t -> t
numerator (Fraction q _) = q
{-# INLINE numerator #-}

denominator :: Fraction t -> t
denominator (Fraction _ p) = p
{-# INLINE denominator #-}

instance (GCDDomain d, DecidableUnits d, UnitNormalForm d) => ZeroProductSemiring (Fraction d)
instance (Eq d, GCDDomain d, DecidableUnits d, UnitNormalForm d) => Eq (Fraction d) where
  Fraction p q == Fraction s t = p*t == q*s
  {-# INLINE (==) #-}

instance (Ord d, GCDDomain d, DecidableUnits d, UnitNormalForm d) => Ord (Fraction d)  where
  compare (Fraction p q) (Fraction p' q') = compare (p*q') (p'*q)
  {-# INLINE compare #-}

instance (GCDDomain d, DecidableUnits d, UnitNormalForm d) => Division (Fraction d) where
  recip (Fraction p q) = let (recipUnit -> Just u, p') = splitUnit p
                         in Fraction (q * u) p'
  Fraction p q / Fraction s t = (p*t) % (q*s)
  {-# INLINE recip #-}
  {-# INLINE (/) #-}

instance (Commutative d, GCDDomain d, DecidableUnits d, UnitNormalForm d) => Commutative (Fraction d)

instance (GCDDomain d, DecidableUnits d, UnitNormalForm d, DecidableZero d) => DecidableZero (Fraction d) where
  isZero (Fraction p _) = isZero p
  {-# INLINE isZero #-}

instance (GCDDomain d, DecidableUnits d, UnitNormalForm d, DecidableZero d) => DecidableUnits (Fraction d) where
  isUnit (Fraction p _) = not $ isZero p
  {-# INLINE isUnit #-}
  recipUnit (Fraction p q) | isZero p  = Nothing
                           | otherwise = Just (Fraction q p)
  {-# INLINE recipUnit #-}
instance (GCDDomain d, DecidableUnits d, UnitNormalForm d) => Ring (Fraction d)
instance (GCDDomain d, DecidableUnits d, UnitNormalForm d) => Abelian (Fraction d)
instance (GCDDomain d, DecidableUnits d, UnitNormalForm d) => Semiring (Fraction d)
instance (GCDDomain d, DecidableUnits d, UnitNormalForm d) => Group (Fraction d) where
  negate (Fraction p q) = Fraction (negate p) q
  Fraction p q - Fraction p' q' = (p*q'-p'*q) % (q*q')
instance (GCDDomain d, DecidableUnits d, UnitNormalForm d) => Monoidal (Fraction d) where
  zero = Fraction zero one
  {-# INLINE zero #-}
instance (GCDDomain d, DecidableUnits d, UnitNormalForm d) => LeftModule Integer (Fraction d) where
  n .* Fraction p r = (n .* p) % r
  {-# INLINE (.*) #-}
instance (GCDDomain d, DecidableUnits d, UnitNormalForm d) => RightModule Integer (Fraction d) where
  Fraction p r *. n = (p *. n) % r
  {-# INLINE (*.) #-}
instance (GCDDomain d, DecidableUnits d, UnitNormalForm d) => LeftModule Natural (Fraction d) where
  n .* Fraction p r = (n .* p) % r
  {-# INLINE (.*) #-}
instance (GCDDomain d, DecidableUnits d, UnitNormalForm d) => RightModule Natural (Fraction d) where
  Fraction p r *. n = (p *. n) % r
  {-# INLINE (*.) #-}
instance (GCDDomain d, DecidableUnits d, UnitNormalForm d) => Additive (Fraction d) where
  Fraction p q + Fraction s t =
    let n = p*t + s*q
        d = q*t
        (n',d') = reduceFraction n d
    in Fraction n' d'
  {-# INLINE (+) #-}
instance (GCDDomain d, DecidableUnits d, UnitNormalForm d) => Unital (Fraction d) where
  one = Fraction one one
  {-# INLINE one #-}
instance (GCDDomain d, DecidableUnits d, UnitNormalForm d) => Multiplicative (Fraction d) where
  Fraction p q * Fraction s t = (p*s) % (q*t)
instance (GCDDomain d, DecidableUnits d, UnitNormalForm d) => Rig (Fraction d)

instance (Characteristic d, GCDDomain d, DecidableUnits d, UnitNormalForm d) => Characteristic (Fraction d) where
  char _ = char (Proxy :: Proxy d)
