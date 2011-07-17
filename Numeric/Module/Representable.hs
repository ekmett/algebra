{-# LANGUAGE RebindableSyntax #-}
module Numeric.Module.Representable 
  ( liftRep, liftRep2
  -- * Representable Additive
  , addRep, replicate1pRep
  -- * Representable Monoidal
  , zeroRep, replicateRep
  -- * Representable Group
  , negateRep, minusRep, subtractRep, timesRep
  -- * Representable Multiplicative (via Algebra)
  , mulRep
  -- * Representable Unital (via UnitalAlgebra)
  , oneRep
  -- * Representable Rig (via Algebra)
  , fromNatural 
  -- * Representable Ring (via Algebra)
  , fromInteger
  ) where

import Numeric.Algebra.Class
import Numeric.Algebra.Unital
import Numeric.Natural.Internal
import Numeric.Additive
import Numeric.Multiplicative
import Numeric.Multiplication.Unital
import Data.Functor.Representable

-- | `Additive.(+)` default definition
addRep :: (Zip m, Additive r) => m r -> m r -> m r
addRep = zipWith (+)

-- | `Additive.replicate1p` default definition
replicate1pRep :: (Whole n, Functor m, Additive r) => n -> m r -> m r
replicate1pRep = fmap . replicate1p

-- | `Monoidal.zero` default definition
zeroRep :: (Applicative m, Monoidal r) => m r 
zeroRep = pure zero

-- | `Monoidal.replicate` default definition
replicateRep :: (Whole n, Functor m, Additive r) => n -> m r -> m r
replicateRep = fmap . replicate

-- | `Group.negate` default definition
negateRep :: (Functor m, Group r) => m r -> m r
negateRep = fmap negate

-- | `Group.(-)` default definition
minusRep :: (Zip m, Group r) => m r -> m r -> m r
minusRep = zipWith (-)

-- | `Group.subtract` default definition
subtractRep :: (Zip m, Group r) => m r -> m r -> m r
subtractRep = zipWith subtract

-- | `Group.times` default definition
timesRep :: (Integral n, Functor m, Group r) => n -> m r -> m r
timesRep = fmap . times

-- | `Multiplicative.(*)` default definition
mulRep :: (Representable m, Algebra r (Key m)) => m r -> m r -> m r
mulRep m n = tabulate $ mult (\b1 b2 -> m b1 * n b2)

-- | `Unital.one` default definition
oneRep :: (Representable m, Unital r, UnitalAlgebra r (Key m)) => m r
oneRep = tabulate $ unit one

-- | `Rig.fromNatural` default definition
fromNaturalRep :: (Representable m, Rig r) => Natural -> m r
fromNaturalRep n = tabulate $ unit (fromNatural n)

-- | `Ring.fromInteger` default definition
fromIntegerRep :: (Representable m, Ring r) => Integer -> m r
fromIntegerRep n = tabulate $ unit (fromNatural n)
