module Numeric.Ring
  ( Ring(..)
  ) where

import Data.Int
import Data.Word
import Numeric.Rig
import Numeric.Rng
import Numeric.Additive.Group
import Numeric.Multiplicative.Monoid

class (Rig r, Rng r) => Ring r where
  fromIntegral :: Integer -> r
  fromIntegral n = times n one

instance Ring Integer where fromIntegral = Prelude.fromIntegral
instance Ring Int     where fromIntegral = Prelude.fromIntegral
instance Ring Int8    where fromIntegral = Prelude.fromIntegral
instance Ring Int16   where fromIntegral = Prelude.fromIntegral
instance Ring Int32   where fromIntegral = Prelude.fromIntegral
instance Ring Int64   where fromIntegral = Prelude.fromIntegral
instance Ring Word    where fromIntegral = Prelude.fromIntegral
instance Ring Word8   where fromIntegral = Prelude.fromIntegral
instance Ring Word16  where fromIntegral = Prelude.fromIntegral
instance Ring Word32  where fromIntegral = Prelude.fromIntegral
instance Ring Word64  where fromIntegral = Prelude.fromIntegral
