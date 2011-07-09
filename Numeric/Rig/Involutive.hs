module Numeric.Rig.Involutive
  ( InvolutiveRig 
  ) where

import Data.Int
import Data.Word
import Numeric.Natural
import Numeric.Multiplication.Involutive
import Numeric.Rig.Class

-- | adjoint (x + y) = adjoint x + adjoint y
class (Rig r, Involutive r) => InvolutiveRig r

instance InvolutiveRig Integer
instance InvolutiveRig Int
instance InvolutiveRig Int8
instance InvolutiveRig Int16
instance InvolutiveRig Int32
instance InvolutiveRig Int64

instance InvolutiveRig Natural
instance InvolutiveRig Word
instance InvolutiveRig Word8
instance InvolutiveRig Word16
instance InvolutiveRig Word32
instance InvolutiveRig Word64
