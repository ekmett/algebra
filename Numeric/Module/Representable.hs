{-# LANGUAGE RebindableSyntax, FlexibleContexts #-}
module Numeric.Module.Representable 
  ( 
  -- * Representable Additive
    addRep, sinnum1pRep
  -- * Representable Monoidal
  , zeroRep, sinnumRep
  -- * Representable Group
  , negateRep, minusRep, subtractRep, timesRep
  -- * Representable Multiplicative (via Algebra)
  , mulRep
  -- * Representable Unital (via UnitalAlgebra)
  , oneRep
  -- * Representable Rig (via Algebra)
  , fromNaturalRep
  -- * Representable Ring (via Algebra)
  , fromIntegerRep
  ) where

import Control.Applicative
import Data.Functor
import Data.Functor.Representable
import Data.Key
import Numeric.Additive.Class
import Numeric.Additive.Group
import Numeric.Algebra.Class
import Numeric.Algebra.Unital
import Numeric.Natural.Internal
import Numeric.Rig.Class
import Numeric.Ring.Class
import Control.Category
import Prelude (($), Integral(..),Integer)

-- | `Additive.(+)` default definition
addRep :: (Zip m, Additive r) => m r -> m r -> m r
addRep = zipWith (+)

-- | `Additive.sinnum1p` default definition
sinnum1pRep :: (Whole n, Functor m, Additive r) => n -> m r -> m r
sinnum1pRep = fmap . sinnum1p

-- | `Monoidal.zero` default definition
zeroRep :: (Applicative m, Monoidal r) => m r 
zeroRep = pure zero

-- | `Monoidal.sinnum` default definition
sinnumRep :: (Whole n, Functor m, Monoidal r) => n -> m r -> m r
sinnumRep = fmap . sinnum

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
mulRep m n = tabulate $ mult (\b1 b2 -> index m b1 * index n b2)

-- | `Unital.one` default definition
oneRep :: (Representable m, Unital r, UnitalAlgebra r (Key m)) => m r
oneRep = tabulate $ unit one

-- | `Rig.fromNatural` default definition
fromNaturalRep :: (UnitalAlgebra r (Key m), Representable m, Rig r) => Natural -> m r
fromNaturalRep n = tabulate $ unit (fromNatural n)

-- | `Ring.fromInteger` default definition
fromIntegerRep :: (UnitalAlgebra r (Key m), Representable m, Ring r) => Integer -> m r
fromIntegerRep n = tabulate $ unit (fromInteger n)
