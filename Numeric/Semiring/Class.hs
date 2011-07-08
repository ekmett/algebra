module Numeric.Semiring.Class
  ( Semiring
  ) where

import Numeric.Additive.Class
import Numeric.Additive.Abelian.Class
import Numeric.Multiplicative.Class
import Data.Int
import Data.Word

-- | A pair of an additive abelian semigroup, and a multiplicative semigroup, with the distributive laws:
-- 
-- > a(b + c) = ab + ac
-- > (a + b)c = ac + bc

class (Additive r, Abelian r, Multiplicative r) => Semiring r

instance Semiring Integer
instance Semiring Bool
instance Semiring Int
instance Semiring Int8
instance Semiring Int16
instance Semiring Int32
instance Semiring Int64
instance Semiring Word
instance Semiring Word8
instance Semiring Word16
instance Semiring Word32
instance Semiring Word64
