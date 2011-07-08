module Numeric.Multiplicative.Commutative.Class where

import Data.Int
import Data.Word
import Numeric.Multiplicative.Class

-- | A commutative multiplicative semigroup
class Multiplicative r => Commutative r

instance Commutative Bool
instance Commutative Integer
instance Commutative Int
instance Commutative Int8
instance Commutative Int16
instance Commutative Int32
instance Commutative Int64
instance Commutative Word
instance Commutative Word8
instance Commutative Word16
instance Commutative Word32
instance Commutative Word64
