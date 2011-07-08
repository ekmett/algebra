module Numeric.Additive.Partitionable.Class
  ( -- * Partitionable Additive Semigroups
    Partitionable(..)
  ) where

import qualified Prelude
import Prelude (Integer,(-))
import Numeric.Additive.Monoid.Class

class AdditiveMonoid m => Partitionable m where
  -- | partitionWith f c returns a list containing f a b for each a b such that a + b = c, 
  partitionWith :: (m -> m -> r) -> m -> [r]

-- pleasant fiction. really, the partitionable set is the naturals
instance Partitionable Integer where
  partitionWith f n = [ f k (n - k) | k <- [0..n] ]
