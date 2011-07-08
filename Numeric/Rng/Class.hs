module Numeric.Rng.Class
  ( Rng
  ) where

import Numeric.Additive.Group.Class
import Numeric.Additive.Abelian.Class
import Numeric.Multiplicative.Class
import Data.Int
import Data.Word

-- | A Ring without an /i/dentity.
-- 
-- > a(b + c) = ab + ac
-- > (a + b)c = ac + bc

class (AdditiveGroup r, Abelian r, Multiplicative r) => Rng r where

instance Rng Integer
instance Rng Int
instance Rng Int8
instance Rng Int16
instance Rng Int32
instance Rng Int64
instance Rng Word
instance Rng Word8
instance Rng Word16
instance Rng Word32
instance Rng Word64
