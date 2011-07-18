{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Numeric.Dioid.Class 
  ( Dioid
  ) where

import Numeric.Additive.Class
import Numeric.Algebra.Class

class (Semiring r, Idempotent r) => Dioid r
instance (Semiring r, Idempotent r) => Dioid r
