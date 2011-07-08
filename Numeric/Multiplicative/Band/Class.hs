module Numeric.Multiplicative.Band.Class
  ( 
  -- * Multiplicative Bands
    Band
  , powBand
  , powBandMonoid
  ) where

import Numeric.Multiplicative.Class
import Numeric.Multiplicative.Monoid.Class

-- | An multiplicative semigroup with idempotent multiplication.
--
-- > a * a = a
class Multiplicative r => Band r

powBand :: (Band r, Integral n) => r -> n -> r
powBand x n
  | n < 1 = error "powBand: positive multiplier required"
  | otherwise = x

powBandMonoid :: (Band r, MultiplicativeMonoid r, Integral n) => r -> n -> r
powBandMonoid x n = case compare n 0 of
  LT -> error "powBandMonoid: non-negative multiplier expected"
  EQ -> one
  GT -> x

instance Band Bool
