{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Numeric.Rng.Class
  ( Rng
  ) where

import Numeric.Group
import Numeric.Semiring

-- | A Ring without an /i/dentity.

class (Group r, Semiring r) => Rng r
instance (Group r, Semiring r) => Rng r
