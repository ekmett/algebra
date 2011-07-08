module Numeric.Semiring
  ( Semiring
  ) where

import Numeric.Additive.Class
import Numeric.Additive.Abelian
import Numeric.Multiplicative.Class
import Numeric.Natural.Internal
import Data.Int
import Data.Word

-- | A pair of an additive abelian semigroup, and a multiplicative semigroup, with the distributive laws:
-- 
-- > a(b + c) = ab + ac
-- > (a + b)c = ac + bc
--
-- Common notation includes the laws for additive and multiplicative identity in semiring.
--
-- We call that a 'Rig' instead, because it makes the connection between Semiring and Ring
-- analogous to Semigroup and Group, and with the sheer number of classes we're tossing around
-- we can use all the mnemonic devices we can get!

class (Additive r, Abelian r, Multiplicative r) => Semiring r

instance Semiring Integer
instance Semiring Natural
instance Semiring Bool
instance Semiring Int
instance Semiring Int8
instance Semiring Int16
instance Semiring Int32
instance Semiring Int64
instance Semiring Word
instance Semiring Word8
instance Semiring Word16
instance Semiring Word32
instance Semiring Word64
