module Numeric.Algebra.Distinguished.Class
  ( Distinguished(..)
  ) where

import Numeric.Covector

-- a basis with a distinguished point
class Distinguished t where
  e :: t

instance Distinguished a => Distinguished (Covector r a) where
  e = return e
