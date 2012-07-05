{-# LANGUAGE 
    MultiParamTypeClasses, 
    GeneralizedNewtypeDeriving, 
    BangPatterns,
    TypeOperators,
    DeriveDataTypeable,
    FlexibleInstances,
    TypeFamilies,
    PatternGuards,
    UndecidableInstances,
    ScopedTypeVariables #-}

module Numeric.Coalgebra.Geometric
  ( 
  -- * Geometric coalgebra primitives
    BasisCoblade(..)
  , Comultivector
  -- * Operations over an eigenbasis
  , Eigenbasis(..)
  , Eigenmetric(..)
  , Euclidean(..)
  -- * Grade
  , grade
  , filterGrade
  -- * Inversions
  , reverse
  , gradeInversion
  , cliffordConjugate
  -- * Products
  , geometric
  , outer
  -- * Inner products
  , contractL
  , contractR
  , hestenes
  , dot
  , liftProduct
  ) where

import Control.Monad (mfilter)
import Data.Bits
import Data.Functor.Representable.Trie
import Data.Word
import Data.Data
import Data.Ix
import Data.Array.Unboxed
import Numeric.Algebra
import Prelude hiding ((-),(*),(+),negate,reverse)

-- a basis vector for a simple geometric coalgebra with the Euclidean inner product
newtype BasisCoblade m = BasisCoblade { runBasisCoblade :: Word64 } deriving 
  ( Eq,Ord,Num,Bits,Enum,Ix,Bounded,Show,Read,Real,Integral
  , Additive,Abelian,LeftModule Natural,RightModule Natural,Monoidal
  , Multiplicative,Unital,Commutative
  , Semiring,Rig
  , DecidableZero,DecidableAssociates,DecidableUnits
  )

instance HasTrie (BasisCoblade m) where
  type BaseTrie (BasisCoblade m) = BaseTrie Word64
  embedKey = embedKey . runBasisCoblade
  projectKey = BasisCoblade . projectKey

-- A metric space over an eigenbasis
class Eigenbasis m where
  euclidean     :: proxy m -> Bool
  antiEuclidean :: proxy m -> Bool
  v             :: m -> BasisCoblade m
  e             :: Int -> m

-- assuming n /= 0, find the index of the least significant set bit in a basis blade
lsb :: BasisCoblade m -> Int
lsb n = fromIntegral $ ix ! shiftR ((n .&. (-n)) * 0x07EDD5E59A4E28C2) 58
  where 
    -- a 64 bit deBruijn multiplication table
    ix :: UArray (BasisCoblade m) Word8
    ix = listArray (0, 63)
      [ 63,  0, 58,  1, 59, 47, 53,  2
      , 60, 39, 48, 27, 54, 33, 42,  3
      , 61, 51, 37, 40, 49, 18, 28, 20
      , 55, 30, 34, 11, 43, 14, 22,  4
      , 62, 57, 46, 52, 38, 26, 32, 41
      , 50, 36, 17, 19, 29, 10, 13, 21
      , 56, 45, 25, 31, 35, 16,  9, 12
      , 44, 24, 15,  8, 23,  7,  6,  5
      ]

class (Ring r, Eigenbasis m) => Eigenmetric r m where
  metric :: m -> r

type Comultivector r m = Covector r (BasisCoblade m)

-- Euclidean basis, we can work with basis vectors for euclidean spaces of up to 64 dimensions without 
-- expanding the representation of our basis vectors
newtype Euclidean = Euclidean Int deriving 
  ( Eq,Ord,Show,Read,Num,Ix,Enum,Real,Integral
  , Data,Typeable
  , Additive,LeftModule Natural,RightModule Natural,Monoidal,Abelian,LeftModule Integer,RightModule Integer,Group
  , Multiplicative,TriviallyInvolutive,InvolutiveMultiplication,InvolutiveSemiring,Unital,Commutative
  , Semiring,Rig,Ring
  )

instance HasTrie Euclidean where
  type BaseTrie Euclidean = BaseTrie Int
  embedKey (Euclidean i) = embedKey i
  projectKey = Euclidean . projectKey

instance Eigenbasis Euclidean where
  euclidean _ = True
  antiEuclidean _ = False
  v n = shiftL 1 (fromIntegral n)
  e = fromIntegral

instance Ring r => Eigenmetric r Euclidean where
  metric _ = one

grade :: BasisCoblade m -> Int
grade = fromIntegral . count 5 . count 4 . count 3 . count 2 . count 1 . count 0 where 
  count c x = (x .&. mask) + (shiftR x p .&. mask) where 
    p = shiftL 1 c
    mask = (-1) `div` (shiftL 1 p + 1)

m1powTimes :: (Bits n, Group r) => n -> r -> r
m1powTimes n r 
  | (n .&. 1) == 0 = r
  | otherwise      = negate r

reorder :: Group r => BasisCoblade m -> BasisCoblade m -> r -> r
reorder a0 b = m1powTimes $ go 0 (shiftR a0 1)
  where
    go !acc 0 = acc
    go acc a = go (acc + grade (a .&. b)) (shiftR a 1)

-- <A>_k
filterGrade :: Monoidal r => BasisCoblade m -> Int -> Comultivector r m
filterGrade b k | grade b == k = zero
                | otherwise    = return b

instance Eigenmetric r m => Coalgebra r (BasisCoblade m) where
  comult f n m = scale (n .&. m) $ reorder n m $ f $ xor n m where
    scale b
      | euclidean n = id
      | otherwise   = (go one b *)
    go :: Eigenmetric r m => r -> BasisCoblade m -> r
    go acc 0 = acc
    go acc n' | b <- lsb n'
              , m' <- metric (e b :: m)
              = go (acc*m') (clearBit n' b)

instance Eigenmetric r m => CounitalCoalgebra r (BasisCoblade m) where
  counit f = f (BasisCoblade zero)

-- instance Group r => InvertibleModule r BasisCoblade where
  
-- reversion (A~) is an involution for the outer product
reverse :: Group r => BasisCoblade m -> Comultivector r m
reverse b = shiftR (g * (g - 1)) 1 `m1powTimes` return b where
  g = grade b

cliffordConjugate :: Group r => BasisCoblade m -> Comultivector r m
cliffordConjugate b = shiftR (g * (g + 1)) 1 `m1powTimes` return b where
  g = grade b

-- A^
gradeInversion :: Group r => BasisCoblade m -> Comultivector r m
gradeInversion b = grade b `m1powTimes` return b

geometric :: Eigenmetric r m => BasisCoblade m -> BasisCoblade m -> Comultivector r m  
geometric = multM

outer :: Eigenmetric r m => BasisCoblade m -> BasisCoblade m -> Comultivector r m
outer m n | m .&. n == 0 = geometric m n 
          | otherwise    = zero

-- A _| B
-- grade (A _| B) = grade B - grade A
contractL :: Eigenmetric r m => BasisCoblade m -> BasisCoblade m -> Comultivector r m 
contractL a b 
  | ga Prelude.> gb   = zero
  | otherwise = mfilter (\r -> grade r == gb - ga) (geometric a b)
  where
    ga = grade a
    gb = grade b

-- A |_ B
-- grade (A |_ B) = grade A - grade B
contractR :: Eigenmetric r m => BasisCoblade m -> BasisCoblade m -> Comultivector r m
contractR a b 
  | ga Prelude.< gb   = zero
  | otherwise = mfilter (\r -> grade r == ga - gb) (geometric a b)
  where
    ga = grade a
    gb = grade b

-- the modified Hestenes' product
dot :: Eigenmetric r m => BasisCoblade m -> BasisCoblade m -> Comultivector r m
dot a b = mfilter (\r -> grade r == abs(grade a - grade b)) (geometric a b)

-- Hestenes' inner product
-- if 0 /= grade a <= grade b then 
-- dot a b = hestenes a b = leftContract a b
hestenes :: Eigenmetric r m => BasisCoblade m -> BasisCoblade m -> Comultivector r m
hestenes a b
  | ga == 0 || gb == 0 = zero
  | otherwise = mfilter (\r -> grade r == abs(ga - gb)) (geometric a b)
  where
    ga = grade a
    gb = grade b

liftProduct :: (BasisCoblade m -> BasisCoblade m -> Comultivector r m) -> Comultivector r m -> Comultivector r m -> Comultivector r m
liftProduct f ma mb = do
  a <- ma
  b <- mb
  f a b
