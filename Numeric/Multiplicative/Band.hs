module Numeric.Multiplicative.Band
  ( 
  -- * Multiplicative Bands
    Band
  , pow1pBand
  , powBand
  ) where

import Numeric.Multiplicative.Class
import Numeric.Multiplicative.Monoid
import Numeric.Natural

-- | An multiplicative semigroup with idempotent multiplication.
--
-- > a * a = a
class Multiplicative r => Band r

pow1pBand :: (Band r, Whole n) => r -> n -> r
pow1pBand r _ = r 

powBand :: (MultiplicativeMonoid r, Band r, Whole n) => r -> n -> r
powBand _ 0 = one
powBand r _ = r

instance Band Bool
