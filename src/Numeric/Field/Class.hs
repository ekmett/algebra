{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Numeric.Field.Class 
  ( Field
  ) where

import Numeric.Algebra.Division
import Numeric.Domain.Euclidean

class (Euclidean d, Division d) => Field d
instance (Euclidean d, Division d) => Field d
