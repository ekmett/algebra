module Numeric.Semiring.Involutive
  ( InvolutiveSemiring
  ) where

import Data.Int
import Data.Word
import Numeric.Natural
import Numeric.Multiplication.Involutive
import Numeric.Semiring.Class

-- | adjoint (x + y) = adjoint x + adjoint y
class (Semiring r, InvolutiveMultiplication r) => InvolutiveSemiring r

instance InvolutiveSemiring Integer
instance InvolutiveSemiring Int
instance InvolutiveSemiring Int8
instance InvolutiveSemiring Int16
instance InvolutiveSemiring Int32
instance InvolutiveSemiring Int64

instance InvolutiveSemiring Natural
instance InvolutiveSemiring Word
instance InvolutiveSemiring Word8
instance InvolutiveSemiring Word16
instance InvolutiveSemiring Word32
instance InvolutiveSemiring Word64

instance InvolutiveSemiring ()
instance (InvolutiveSemiring a, InvolutiveSemiring b) => InvolutiveSemiring (a, b)
instance (InvolutiveSemiring a, InvolutiveSemiring b, InvolutiveSemiring c) => InvolutiveSemiring (a, b, c)
instance (InvolutiveSemiring a, InvolutiveSemiring b, InvolutiveSemiring c, InvolutiveSemiring d) => InvolutiveSemiring (a, b, c, d)
instance (InvolutiveSemiring a, InvolutiveSemiring b, InvolutiveSemiring c, InvolutiveSemiring d, InvolutiveSemiring e) => InvolutiveSemiring (a, b, c, d, e)
