module Numeric.Ring.Commutative.Class
  ( CommutativeRing
  ) where

import Data.Int
import Data.Word
import Numeric.Ring.Class

class Ring r => CommutativeRing r where

instance CommutativeRing Int
instance CommutativeRing Int8
instance CommutativeRing Int16
instance CommutativeRing Int32
instance CommutativeRing Int64
instance CommutativeRing Integer
instance CommutativeRing Word
instance CommutativeRing Word8
instance CommutativeRing Word16
instance CommutativeRing Word32
instance CommutativeRing Word64
