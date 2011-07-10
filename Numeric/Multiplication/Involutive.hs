module Numeric.Multiplication.Involutive
  ( InvolutiveMultiplication(..)
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
class Multiplicative r => InvolutiveMultiplication r where
  adjoint :: r -> r

adjointCommutative :: Commutative r => r -> r
adjointCommutative = id

instance InvolutiveMultiplication Int where adjoint = id
instance InvolutiveMultiplication Integer where adjoint = id
instance InvolutiveMultiplication Int8 where adjoint = id
instance InvolutiveMultiplication Int16 where adjoint = id
instance InvolutiveMultiplication Int32 where adjoint = id
instance InvolutiveMultiplication Int64 where adjoint = id
instance InvolutiveMultiplication Bool where adjoint = id
instance InvolutiveMultiplication Word where adjoint = id
instance InvolutiveMultiplication Natural where adjoint = id
instance InvolutiveMultiplication Word8 where adjoint = id
instance InvolutiveMultiplication Word16 where adjoint = id
instance InvolutiveMultiplication Word32 where adjoint = id
instance InvolutiveMultiplication Word64 where adjoint = id
instance InvolutiveMultiplication () where adjoint = id
instance (InvolutiveMultiplication a, InvolutiveMultiplication b) => InvolutiveMultiplication (a,b) where
  adjoint (a,b) = (adjoint a, adjoint b)
instance (InvolutiveMultiplication a, InvolutiveMultiplication b, InvolutiveMultiplication c) => InvolutiveMultiplication (a,b,c) where
  adjoint (a,b,c) = (adjoint a, adjoint b, adjoint c)
instance (InvolutiveMultiplication a, InvolutiveMultiplication b, InvolutiveMultiplication c, InvolutiveMultiplication d) => InvolutiveMultiplication (a,b,c,d) where
  adjoint (a,b,c,d) = (adjoint a, adjoint b, adjoint c, adjoint d)
instance (InvolutiveMultiplication a, InvolutiveMultiplication b, InvolutiveMultiplication c, InvolutiveMultiplication d, InvolutiveMultiplication e) => InvolutiveMultiplication (a,b,c,d,e) where
  adjoint (a,b,c,d,e) = (adjoint a, adjoint b, adjoint c, adjoint d, adjoint e)
