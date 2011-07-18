{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module Numeric.Multiplication.Involutive.Internal
  ( InvolutiveMultiplication(..)
  , TriviallyInvolutive
  , InvolutiveAlgebra(..)
  , TriviallyInvolutiveAlgebra
  ) where

import Data.Int
import Data.Word
import Numeric.Natural.Internal
import Numeric.Multiplicative
import Numeric.Multiplication.Commutative
import Numeric.Algebra.Class
import Numeric.Algebra.Commutative

-- | An semigroup with involution
-- 
-- > adjoint a * adjoint b = adjoint (b * a)
class Multiplicative r => InvolutiveMultiplication r where
  adjoint :: r -> r

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

-- adjoint = id
class (Commutative r, InvolutiveMultiplication r) => TriviallyInvolutive r
instance TriviallyInvolutive Int
instance TriviallyInvolutive Integer
instance TriviallyInvolutive Int8
instance TriviallyInvolutive Int16
instance TriviallyInvolutive Int32
instance TriviallyInvolutive Int64
instance TriviallyInvolutive Bool
instance TriviallyInvolutive Word
instance TriviallyInvolutive Natural
instance TriviallyInvolutive Word8
instance TriviallyInvolutive Word16
instance TriviallyInvolutive Word32
instance TriviallyInvolutive Word64
instance TriviallyInvolutive ()
instance (TriviallyInvolutive a, TriviallyInvolutive b) => TriviallyInvolutive (a,b)
instance (TriviallyInvolutive a, TriviallyInvolutive b, TriviallyInvolutive c) => TriviallyInvolutive (a,b,c)
instance (TriviallyInvolutive a, TriviallyInvolutive b, TriviallyInvolutive c, TriviallyInvolutive d) => TriviallyInvolutive (a,b,c,d)
instance (TriviallyInvolutive a, TriviallyInvolutive b, TriviallyInvolutive c, TriviallyInvolutive d, TriviallyInvolutive e) => TriviallyInvolutive (a,b,c,d,e)

-- inv is an associative algebra homomorphism
class Algebra r a => InvolutiveAlgebra r a where
  inv :: (a -> r) -> a -> r

instance InvolutiveAlgebra () a where
  inv _ _ = ()
instance (Algebra r b, InvolutiveAlgebra r a) => InvolutiveAlgebra (b -> r) a where
  inv f c a = inv (`f` a) c
instance (InvolutiveAlgebra r a, InvolutiveAlgebra r b) => InvolutiveAlgebra r (a, b) where
  inv f (a,b) = inv (\a' -> inv (\b' -> f (a',b')) b) a
instance (InvolutiveAlgebra r a, InvolutiveAlgebra r b, InvolutiveAlgebra r c) => InvolutiveAlgebra r (a, b, c) where
  inv f (a,b,c) = inv (\a' -> inv (\b' -> inv (\c' -> f (a',b',c')) c) b) a
instance (InvolutiveAlgebra r a, InvolutiveAlgebra r b, InvolutiveAlgebra r c, InvolutiveAlgebra r d) => InvolutiveAlgebra r (a, b, c, d) where
  inv f (a,b,c,d) = inv (\a' -> inv (\b' -> inv (\c' -> inv (\d' -> f (a',b',c',d')) d) c) b) a
instance (InvolutiveAlgebra r a, InvolutiveAlgebra r b, InvolutiveAlgebra r c, InvolutiveAlgebra r d, InvolutiveAlgebra r e) => InvolutiveAlgebra r (a, b, c, d, e) where
  inv f (a,b,c,d,e) = inv (\a' -> inv (\b' -> inv (\c' -> inv (\d' -> inv (\e' -> f (a',b',c',d',e')) e) d) c) b) a
instance InvolutiveAlgebra r h => InvolutiveMultiplication (h -> r) where
  adjoint = inv

class (CommutativeAlgebra r a, InvolutiveAlgebra r a) => TriviallyInvolutiveAlgebra r a

instance TriviallyInvolutiveAlgebra () a 
instance (Algebra r b, TriviallyInvolutiveAlgebra r a) => TriviallyInvolutiveAlgebra (b -> r) a
instance (TriviallyInvolutiveAlgebra r a, TriviallyInvolutiveAlgebra r b) => TriviallyInvolutiveAlgebra r (a, b) where
instance (TriviallyInvolutiveAlgebra r a, TriviallyInvolutiveAlgebra r b, TriviallyInvolutiveAlgebra r c) => TriviallyInvolutiveAlgebra r (a, b, c) where
instance (TriviallyInvolutiveAlgebra r a, TriviallyInvolutiveAlgebra r b, TriviallyInvolutiveAlgebra r c, TriviallyInvolutiveAlgebra r d) => TriviallyInvolutiveAlgebra r (a, b, c, d)
instance (TriviallyInvolutiveAlgebra r a, TriviallyInvolutiveAlgebra r b, TriviallyInvolutiveAlgebra r c, TriviallyInvolutiveAlgebra r d, TriviallyInvolutiveAlgebra r e) => TriviallyInvolutiveAlgebra r (a, b, c, d, e)
instance TriviallyInvolutiveAlgebra r h => TriviallyInvolutive (h -> r)

