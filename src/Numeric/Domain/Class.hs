{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Numeric.Domain.Class where
import Numeric.Ring.Class
import Numeric.Semiring.ZeroProduct

-- | (Integral) domain is the integral semiring.
class (ZeroProductSemiring d, Ring d) => Domain d
instance (ZeroProductSemiring d, Ring d) => Domain d
