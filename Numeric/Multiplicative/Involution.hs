module Numeric.Multiplicative.Involution
  ( Involution(..)
  , trivialAdjoint
  ) where

import Data.Int
import Data.Word
import Numeric.Natural.Internal
import Numeric.Multiplicative.Class
import Numeric.Multiplicative.Commutative

-- | An semigroup with involution
-- 
-- > adjoint a * adjoint b = adjoint (b * a)
class Multiplicative r => Involution r where
  adjoint :: r -> r

trivialAdjoint :: Commutative r => r -> r
trivialAdjoint = id

instance Involution Int where adjoint = id
instance Involution Integer where adjoint = id
instance Involution Int8 where adjoint = id
instance Involution Int16 where adjoint = id
instance Involution Int32 where adjoint = id
instance Involution Int64 where adjoint = id
instance Involution Word where adjoint = id
instance Involution Natural where adjoint = id
instance Involution Word8 where adjoint = id
instance Involution Word16 where adjoint = id
instance Involution Word32 where adjoint = id
instance Involution Word64 where adjoint = id
