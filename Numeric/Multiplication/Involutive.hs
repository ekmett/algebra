module Numeric.Multiplication.Involutive
  ( Involutive(..)
  , adjointCommutative
  ) where

import Data.Int
import Data.Word
import Numeric.Natural.Internal
import Numeric.Semigroup.Multiplicative
import Numeric.Multiplication.Commutative

-- | An semigroup with involution
-- 
-- > adjoint a * adjoint b = adjoint (b * a)
class Multiplicative r => Involutive r where
  adjoint :: r -> r

adjointCommutative :: Commutative r => r -> r
adjointCommutative = id

instance Involutive Int where adjoint = id
instance Involutive Integer where adjoint = id
instance Involutive Int8 where adjoint = id
instance Involutive Int16 where adjoint = id
instance Involutive Int32 where adjoint = id
instance Involutive Int64 where adjoint = id
instance Involutive Bool where adjoint = id
instance Involutive Word where adjoint = id
instance Involutive Natural where adjoint = id
instance Involutive Word8 where adjoint = id
instance Involutive Word16 where adjoint = id
instance Involutive Word32 where adjoint = id
instance Involutive Word64 where adjoint = id
instance Involutive () where adjoint = id
instance (Involutive a, Involutive b) => Involutive (a,b) where
  adjoint (a,b) = (adjoint a, adjoint b)
instance (Involutive a, Involutive b, Involutive c) => Involutive (a,b,c) where
  adjoint (a,b,c) = (adjoint a, adjoint b, adjoint c)
instance (Involutive a, Involutive b, Involutive c, Involutive d) => Involutive (a,b,c,d) where
  adjoint (a,b,c,d) = (adjoint a, adjoint b, adjoint c, adjoint d)
instance (Involutive a, Involutive b, Involutive c, Involutive d, Involutive e) => Involutive (a,b,c,d,e) where
  adjoint (a,b,c,d,e) = (adjoint a, adjoint b, adjoint c, adjoint d, adjoint e)
