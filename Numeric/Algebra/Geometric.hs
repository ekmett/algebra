{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, BangPatterns #-}
module Numeric.Algebra.Geometric
  ( 
  -- * geometric algebra primitives
    Blade(..)
  , Multivector
  , Comultivector
  -- * Operations over an eigenbasis
  , Eigenbasis(..)
  , Eigenmetric(..)
  -- * grade
  , grade
  -- * inversions
  , reverse
  , gradeInversion
  , cliffordConjugate
  -- * products
  , geometric
  , outer
  -- * inner products
  , contractL
  , contractR
  , hestenes
  , dot
  ) where

import Control.Monad (mfilter)
import Data.Bits
import Data.Functor.Representable.Trie
import Data.Proxy
import Data.Word
import Data.Ix
import Data.Array.Unboxed
import Numeric.Addition
import Numeric.Multiplication
import Numeric.Decidable.Units
import Numeric.Decidable.Associates
import Numeric.Decidable.Zero
import Numeric.Semiring.Class
import Numeric.Rig.Class
import Numeric.Rng.Class
import Numeric.Ring.Class
import Numeric.Module.Class
import Prelude hiding ((-),(*),(+))

-- a basis vector for a simple geometric algebra with the euclidean inner product
newtype Blade m = Blade { runBlade :: Word64 } deriving 
  (Eq,Ord,Num,Bits,Enum,Ix,Bounded,Show,Read
  ,Additive,Abelian,LeftModule Natural,RightModule Natural,Monoidal
  ,Multiplicative,Unital,Commutative
  ,Semiring,Rig
  ,DecidableZero,DecidableAssociates,DecidableUnits
  ,HasTrie
  )

-- A metric space over an eigenbasis
class Eigenbasis m where
  euclidean     :: proxy m -> Bool
  antiEuclidean :: proxy m -> Bool
  v             :: m -> Blade m
  e             :: Int -> m

  -- assuming n /= 0, find the index of the least significant set bit in a basis blade
  lsb :: Blade m -> m
  lsb n = e $ index ! shiftR ((n .&. negate n) * 0x07EDD5E59A4E28C2) 58
    where 
      -- a 64 bit deBruijn multiplication table
      index :: UArray (Blade m) Int
      index = listArray (0,63) 
	   [ 63,  0, 58,  1, 59, 47, 53,  2
	   , 60, 39, 48, 27, 54, 33, 42,  3
	   , 61, 51, 37, 40, 49, 18, 28, 20
	   , 55, 30, 34, 11, 43, 14, 22,  4
	   , 62, 57, 46, 52, 38, 26, 32, 41
	   , 50, 36, 17, 19, 29, 10, 13, 21
	   , 56, 45, 25, 31, 35, 16,  9, 12
	   , 44, 24, 15,  8, 23,  7,  6,  5
	   ]

class Eigenbasis m => Eigenmetric r m where
  metric :: m -> r

type Comultivector r m = Covector r (Blade m)

type Multivector r m = Blade m :->: r

-- Euclidean basis, we can work with basis vectors for euclidean spaces of up to 64 dimensions without 
-- expanding the representation of our basis vectors
newtype Euclidean = Euclidean Int deriving 
  (Eq,Ord,Show,Read,Num,Ix,Enum,Integral
  ,Data,Typeable
  ,Additive,LeftModule Natural,RightModule Natural,Monoidal,Abelian,LeftModule Integer,RightModule Integer,Group
  ,Multiplicative,Involutive,Unital,Commutative
  ,Semiring,Rig,Rng,Rng
  ,HasTrie
  )

instance Eigenbasis (Euclidean r) where
  euclidean _ = True
  antiEuclidean _ = False
  v n = shiftL (Blade 1) n
  e = fromIntegral

instance Unital r => Eigenmetric r Euclidean where
  metric _ = one

grade :: Blade m -> Int
grade n = fromIntegral . count 5 . count 4 . count 3 . count 2 . count 1 . count 0 where 
  count c x = (x .&. mask) + (shiftR x pow .&. mask) where 
    pow = shiftL 1 c
    mask = (-1 :: Word64) `div` (shiftL 1 pow + 1)

m1powTimes :: (Bits n, Group r) => n -> r -> r
m1powTimes n r 
  | shiftL (n .&. 1) == 0 = r
  | otherwise             = negate r

reorder :: Group r => Blade m -> Blade m -> r -> r
reorder a0 b = m1powTimes $ go 0 (shiftR a0 1)
  where
    go !acc 0 = acc
    go acc a = go (acc + grade (a .&. b)) (shiftR a 1)

-- <A>_k
filterGrade :: Monoidal r => Blade m -> Int -> Covector r (Blade m)
filterGrade b k | grade b == k = zero
                | otherwise    = return b

instance Eigenmetric r m => Coalgebra r (Blade m) where
  comult f n m = scale (n .&. m) $ reorder n m $ f $ xor n m where
    scale i | euclidean n = id
            | otherwise   = (go one i .*)
    go acc 0 = acc
    go acc n | b <- lsb n
             , m <- metric b 
             = go (acc*m) (clearBit n b)

instance (Monoidal r, Eigenmetric r m) => CounitalCoalgebra r (Blade m) where
  counit f = f (Blade zero)

-- instance Group r => InvertibleModule r Blade where
  
-- reversion (A~) is an involution for the outer product
reverse :: Group r => Blade m -> Comultivector r m
reverse b = shiftR (g * (g - 1)) 1 `m1powTimes` return b where
  g = grade b

cliffordConjugate :: Group r => Blade m -> Comultivector r m
cliffordConjugate b = shiftR (g * (g + 1)) 1 `m1powTimes` return b where
  g = grade b

-- A^
gradeInversion :: Group r => Blade m -> Comultivector r m
gradeInversion b = grade b `m1powTimes` return b

geometric :: Eigenmetric r m => Blade m -> Blade m -> Comultivector r m  
geometric = multM

outer :: Eigenmetric r m => Blade m -> Blade m -> Comultivector r m
outer m n | m .&. n == 0 = geometric m n 
          | otherwise    = zero

-- A _| B
-- grade (A _| B) = grade B - grade A
contractL :: Eigenmetric r m => Blade m -> Blade m -> Comultivector r m 
contractL a b 
  | ga > gb   = zero
  | otherwise = mfilter (\r -> grade r == gb - ga) (geometric a b)
  where
    ga = grade a
    gb = grade b

-- A |_ B
-- grade (A |_ B) = grade A - grade B
contractR :: Eigenmetric m => Blade m -> Blade m -> Comultivector r m
contractR a b 
  | ga < gb   = zero
  | otherwise = mfilter (\r -> grade r == ga - gb) (geometric a b)
  where
    ga = grade a
    gb = grade b

-- the modified Hestenes' product
dot :: Eigenmetric r m => Blade m -> Blade m -> Comultivector r m
dot a b = mfilter (\r -> grade r == abs(grade a - grade b)) (geometric a b)

-- Hestenes' inner product
-- if 0 /= grade a <= grade b then 
-- dot a b = hestenes a b = leftContract a b
hestenes :: Eigenmetric r m => Blade m -> Blade m -> Comultivector r m
hestenes a b
  | ga == 0 || gb == 0 = zero
  | otherwise = mfilter (\r -> grade r == abs(ga - gb)) (geometric a b)
  where
    ga = grade a
    gb = grade b

liftProduct :: (Blade m -> Blade m -> Comultivector r m) -> Comultivector r m -> Comultivector r m -> Comultivector r m
liftProduct f ma mb = do
  a <- ma
  b <- mb
  f a b
