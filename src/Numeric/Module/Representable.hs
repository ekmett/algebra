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
import Data.Functor.Rep
import Numeric.Additive.Class
import Numeric.Additive.Group
import Numeric.Algebra.Class
import Numeric.Algebra.Unital
import Numeric.Natural
import Numeric.Rig.Class
import Numeric.Ring.Class
import Control.Category
import Prelude (($), Integral(..),Integer)

-- | `Additive.(+)` default definition
addRep :: (Applicative m, Additive r) => m r -> m r -> m r
addRep = liftA2 (+)

-- | `Additive.sinnum1p` default definition
sinnum1pRep :: (Functor m, Additive r) => Natural -> m r -> m r
sinnum1pRep = fmap . sinnum1p

-- | `Monoidal.zero` default definition
zeroRep :: (Applicative m, Monoidal r) => m r 
zeroRep = pure zero

-- | `Monoidal.sinnum` default definition
sinnumRep :: (Functor m, Monoidal r) => Natural -> m r -> m r
sinnumRep = fmap . sinnum

-- | `Group.negate` default definition
negateRep :: (Functor m, Group r) => m r -> m r
negateRep = fmap negate

-- | `Group.(-)` default definition
minusRep :: (Applicative m, Group r) => m r -> m r -> m r
minusRep = liftA2 (-)

-- | `Group.subtract` default definition
subtractRep :: (Applicative m, Group r) => m r -> m r -> m r
subtractRep = liftA2 subtract

-- | `Group.times` default definition
timesRep :: (Integral n, Functor m, Group r) => n -> m r -> m r
timesRep = fmap . times

-- | `Multiplicative.(*)` default definition
mulRep :: (Representable m, Algebra r (Rep m)) => m r -> m r -> m r
mulRep m n = tabulate $ mult (\b1 b2 -> index m b1 * index n b2)

-- | `Unital.one` default definition
oneRep :: (Representable m, Unital r, UnitalAlgebra r (Rep m)) => m r
oneRep = tabulate $ unit one

-- | `Rig.fromNatural` default definition
fromNaturalRep :: (UnitalAlgebra r (Rep m), Representable m, Rig r) => Natural -> m r
fromNaturalRep n = tabulate $ unit (fromNatural n)

-- | `Ring.fromInteger` default definition
fromIntegerRep :: (UnitalAlgebra r (Rep m), Representable m, Ring r) => Integer -> m r
fromIntegerRep n = tabulate $ unit (fromInteger n)
