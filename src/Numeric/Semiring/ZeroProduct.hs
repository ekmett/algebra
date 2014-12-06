module Numeric.Semiring.ZeroProduct
  ( ZeroProductSemiring
  ) where

import Numeric.Algebra.Class
import Numeric.Natural

-- | A zero-product semiring has no zero divisors
--
-- > a * b = 0 implies a == 0 || b == 0
class (Monoidal r, Semiring r) => ZeroProductSemiring r

instance ZeroProductSemiring Integer
instance ZeroProductSemiring Natural
instance ZeroProductSemiring Bool
