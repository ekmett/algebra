module Numeric.Additive.Abelian.Class
  ( 
  -- * An Additive Abelian Semigroup
    Abelian
  ) where

import Data.Int
import Data.Word
import Numeric.Additive.Class
import Numeric.Natural.Internal

-- | an additive abelian semigroup
--
-- a + b = b + a
class Additive r => Abelian r

instance Abelian r => Abelian (e -> r)
instance Abelian Bool
instance Abelian Integer
instance Abelian Natural
instance Abelian Int
instance Abelian Int8
instance Abelian Int16
instance Abelian Int32
instance Abelian Int64
instance Abelian Word
instance Abelian Word8
instance Abelian Word16
instance Abelian Word32
instance Abelian Word64
