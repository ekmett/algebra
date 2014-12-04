{-# LANGUAGE NoImplicitPrelude #-}

module Numeric.Decidable.Nilpotent (DecidableNilpotent(..)) where

import Data.Bits(Bits(), (.&.), zeroBits)
import Data.Int(Int8,Int16,Int32,Int64)
import Data.Word(Word8,Word16,Word32,Word64)
import Numeric.Algebra
import Numeric.Decidable.Zero
import Prelude hiding (Num(..), Ord(..))

-- | An element `x` is nilpotent if there exists `n` s.t. `pow1p x n` is zero.
class (Monoidal r, Multiplicative r) => DecidableNilpotent r where
    isNilpotent :: r -> Bool

instance DecidableNilpotent () where
    isNilpotent _ = True

instance DecidableNilpotent Bool where
    isNilpotent = isZero
instance DecidableNilpotent Natural where
    isNilpotent = isZero
instance DecidableNilpotent Integer where
    isNilpotent = isZero

instance DecidableNilpotent Int where
    isNilpotent = signedBitsNilpotent
instance DecidableNilpotent Int8 where
    isNilpotent = signedBitsNilpotent
instance DecidableNilpotent Int16 where
    isNilpotent = signedBitsNilpotent
instance DecidableNilpotent Int32 where
    isNilpotent = signedBitsNilpotent
instance DecidableNilpotent Int64 where
    isNilpotent = signedBitsNilpotent
instance DecidableNilpotent Word8 where
    isNilpotent = unsignedBitsNilpotent
instance DecidableNilpotent Word16 where
    isNilpotent = unsignedBitsNilpotent
instance DecidableNilpotent Word32 where
    isNilpotent = unsignedBitsNilpotent
instance DecidableNilpotent Word64 where
    isNilpotent = unsignedBitsNilpotent

instance (DecidableNilpotent a, DecidableNilpotent b) => DecidableNilpotent (a,b) where
    isNilpotent (a,b) = isNilpotent a && isNilpotent b

instance (DecidableNilpotent a, DecidableNilpotent b, DecidableNilpotent c) => DecidableNilpotent (a,b,c) where
    isNilpotent (a,b,c) = isNilpotent a && isNilpotent b && isNilpotent c

instance (DecidableNilpotent a, DecidableNilpotent b, DecidableNilpotent c, DecidableNilpotent d) => DecidableNilpotent (a,b,c,d) where
    isNilpotent (a,b,c,d) = isNilpotent a && isNilpotent b && isNilpotent c && isNilpotent d

instance (DecidableNilpotent a, DecidableNilpotent b, DecidableNilpotent c, DecidableNilpotent d, DecidableNilpotent e) => DecidableNilpotent (a,b,c,d,e) where
    isNilpotent (a,b,c,d,e) = isNilpotent a && isNilpotent b && isNilpotent c && isNilpotent d && isNilpotent e

unsignedBitsNilpotent :: (Bits r, Group r, Unital r) => r -> Bool
unsignedBitsNilpotent b = (b /= one) && b .&. (b - one) == zeroBits

signedBitsNilpotent :: (Bits r, Group r, Order r, Bounded r, Unital r) => r -> Bool
signedBitsNilpotent b | zero <~ b = unsignedBitsNilpotent b
                      | otherwise = b == minBound ||
                                    unsignedBitsNilpotent (negate b)

