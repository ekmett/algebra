module Numeric.Multiplication.Involutive.Internal
  ( InvolutiveMultiplication(..)
  , adjointCommutative
  , InvolutiveAlgebra(..)
  ) where

import Data.Int
import Data.Word
import Numeric.Natural.Internal
import Numeric.Multiplicative
import Numeric.Multiplication.Commutative
import Numeric.Algebra.Class

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

-- inv is an associative algebra homomorphism
class Algebra r a => InvolutiveAlgebra r h where
  inv :: (h -> r) -> h -> r

instance InvolutiveAlgebra () a where
  inv _ _ = ()
instance (Algebra r b, InvolutiveAlgebra r h) => InvolutiveAgebra (b -> r) a where
  inv f c a = inv (`f` a) c
instance (InvolutiveAlgebra r a, InvolutiveAlgebra r b) => InvolutiveAlgebra r (a, b) where
  coinv f (a,b) = coinv (\a' -> coinv (\b' -> f (a',b')) b) a
instance (InvolutiveAlgebra r a, InvolutiveAlgebra r b, InvolutiveAlgebra r c) => InvolutiveAlgebra r (a, b, c) where
  coinv f (a,b,c) = coinv (\a' -> coinv (\b' -> coinv (\c' -> f (a',b',c')) c) b) a
instance (InvolutiveAlgebra r a, InvolutiveAlgebra r b, InvolutiveAlgebra r c, InvolutiveAlgebra r d) => InvolutiveAlgebra r (a, b, c, d) where
  coinv f (a,b,c,d) = coinv (\a' -> coinv (\b' -> coinv (\c' -> coinv (\d' -> f (a',b',c',d')) d) c) b) a
instance (InvolutiveAlgebra r a, InvolutiveAlgebra r b, InvolutiveAlgebra r c, InvolutiveAlgebra r d, InvolutiveAlgebra r e) => InvolutiveAlgebra r (a, b, c, d, e) where
  coinv f (a,b,c,d,e) = coinv (\a' -> coinv (\b' -> coinv (\c' -> coinv (\d' -> coinv (\e' -> f (a',b',c',d',e')) e) d) c) b) a
instance InvolutiveAlgebra r h => InvolutiveMultiplication (h -> r) where
  adjoint = inv
