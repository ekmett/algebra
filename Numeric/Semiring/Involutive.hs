module Numeric.Semiring.Involutive
  ( Involutive 
  ) where

import Data.Int
import Data.Word
import Numeric.Natural
import Numeric.Multiplication.Involutive
import Numeric.Rig.Class

-- | adjoint (x + y) = adjoint x + adjoint y
class (Rig r, InvolutiveMultiplication r) => Involutive r

instance Involutive Integer
instance Involutive Int
instance Involutive Int8
instance Involutive Int16
instance Involutive Int32
instance Involutive Int64

instance Involutive Natural
instance Involutive Word
instance Involutive Word8
instance Involutive Word16
instance Involutive Word32
instance Involutive Word64

instance Involutive ()
instance (Involutive a, Involutive b) => Involutive (a, b)
instance (Involutive a, Involutive b, Involutive c) => Involutive (a, b, c)
instance (Involutive a, Involutive b, Involutive c, Involutive d) => Involutive (a, b, c, d)
instance (Involutive a, Involutive b, Involutive c, Involutive d, Involutive e) => Involutive (a, b, c, d, e)
