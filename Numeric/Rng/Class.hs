{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Numeric.Rng.Class
  ( Rng
  ) where

import Numeric.Additive.Group
import Numeric.Algebra.Class

-- | A Ring without an /i/dentity.

class (Group r, Semiring r) => Rng r
instance (Group r, Semiring r) => Rng r
