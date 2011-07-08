module Numeric.Ring.Class
  ( Ring(..) 
  ) where

import Data.Int
import Data.Word
import Prelude hiding (fromInteger, replicate)
import qualified Prelude
import Numeric.Additive.Semigroup (replicate)
import Numeric.Multiplicative.Monoid (MultiplicativeMonoid(one))
import Numeric.Rng.Class

class (MultiplicativeMonoid r, Rng r) => Ring r where
  fromInteger :: Integer -> r
  fromInteger n = replicate n one

instance Ring Int where fromInteger = Prelude.fromInteger
instance Ring Int8 where fromInteger = Prelude.fromInteger
instance Ring Int16 where fromInteger = Prelude.fromInteger
instance Ring Int32 where fromInteger = Prelude.fromInteger
instance Ring Int64 where fromInteger = Prelude.fromInteger
instance Ring Integer where fromInteger = Prelude.fromInteger
instance Ring Word where fromInteger = Prelude.fromInteger
instance Ring Word8 where fromInteger = Prelude.fromInteger
instance Ring Word16 where fromInteger = Prelude.fromInteger
instance Ring Word32 where fromInteger = Prelude.fromInteger
instance Ring Word64 where fromInteger = Prelude.fromInteger
