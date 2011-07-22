module Numeric.Order.LocallyFinite 
  ( LocallyFiniteOrder(..)
  ) where

import Control.Applicative
import Numeric.Additive.Class
import Numeric.Additive.Group
import Numeric.Algebra.Class
import Numeric.Algebra.Unital
import Numeric.Order.Class
import Numeric.Natural.Internal
import Numeric.Rig.Class
import Numeric.Ring.Class
import Data.Int
import Data.Bits
import Data.Word
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Ix as Ix
import Prelude hiding ((*),(+),fromIntegral,(<),negate,(-))

class Order a => LocallyFiniteOrder a where
  range :: a -> a -> [a]
  rangeSize :: a -> a -> Natural

  -- moebiusInversion inversion
  moebiusInversion :: Ring r => a -> a -> r
  moebiusInversion x y = case order x y of
    Just EQ -> one
    Just LT -> sumWith (\z -> if z < y then moebiusInversion x z else zero) $ range x y
    _  -> zero 

instance LocallyFiniteOrder Natural where
  range = curry Ix.range
  rangeSize a b 
    | a <= b = Natural (runNatural b - runNatural a + 1)
    | otherwise = 0
  moebiusInversion x y = case compare x y of
     EQ -> one
     LT | unsafePred y == x -> negate one 
     _ -> zero

instance LocallyFiniteOrder Integer where
  range = curry Ix.range
  rangeSize a b 
    | a <= b = Natural (b - a + 1)
    | otherwise = 0
  moebiusInversion x y = case compare x y of
     EQ -> one
     LT | y - 1 == x -> negate one 
     _  -> zero

instance Ord a => LocallyFiniteOrder (Set a) where
  range a b 
    | Set.isSubsetOf a b = go a $ Set.toList $ Set.difference b a
    | otherwise = []
    where 
      go _ [] = []
      go s (x:xs) = do
        s' <- [s, Set.insert x s]
        go s' xs
  rangeSize a b 
    | Set.isSubsetOf a b = fromNatural $ shiftL 1 $ Set.size b - Set.size a
    | otherwise = zero
  moebiusInversion a b 
    | Set.isSubsetOf a b = 
      if (Set.size b - Set.size a) .&. 1 == 0 
      then one 
      else negate one
    | otherwise          = zero

instance LocallyFiniteOrder Bool where
  range False False = [False]
  range False True  = [False, True]
  range True  False = []
  range True  True  = [True]
  rangeSize False False = 1
  rangeSize False True  = 2
  rangeSize True  False = 0 
  rangeSize True  True  = 1
  moebiusInversion False False = one
  moebiusInversion False True  = negate one 
  moebiusInversion True  False = zero
  moebiusInversion True  True  = one


instance LocallyFiniteOrder Int where
  range = curry Ix.range
  rangeSize a b
    | a <= b = Natural $ fromIntegral $ b - a + 1
    | otherwise = 0
  moebiusInversion x y = case compare x y of
     EQ -> one
     LT | y - 1 == x -> negate one 
     _  -> zero

instance LocallyFiniteOrder Int8 where
  range = curry Ix.range
  rangeSize a b
    | a <= b = Natural $ fromIntegral $ b - a + 1
    | otherwise = 0
  moebiusInversion x y = case compare x y of
     EQ -> one
     LT | y - 1 == x -> negate one 
     _  -> zero

instance LocallyFiniteOrder Int16 where
  range = curry Ix.range
  rangeSize a b
    | a <= b = Natural $ fromIntegral $ b - a + 1
    | otherwise = 0
  moebiusInversion x y = case compare x y of
     EQ -> one
     LT | y - 1 == x -> negate one 
     _  -> zero

instance LocallyFiniteOrder Int32 where
  range = curry Ix.range
  rangeSize a b
    | a <= b = Natural $ fromIntegral $ b - a + 1
    | otherwise = 0
  moebiusInversion x y = case compare x y of
     EQ -> one
     LT | y - 1 == x -> negate one 
     _  -> zero

instance LocallyFiniteOrder Int64 where
  range = curry Ix.range
  rangeSize a b
    | a <= b = Natural $ fromIntegral $ b - a + 1
    | otherwise = 0
  moebiusInversion x y = case compare x y of
     EQ -> one
     LT | y - 1 == x -> negate one 
     _  -> zero

instance LocallyFiniteOrder Word where
  range = curry Ix.range
  rangeSize a b
    | a <= b = Natural $ fromIntegral $ b - a + 1
    | otherwise = 0
  moebiusInversion x y = case compare x y of
     EQ -> one
     LT | y - 1 == x -> negate one 
     _  -> zero

instance LocallyFiniteOrder Word8 where
  range = curry Ix.range
  rangeSize a b
    | a <= b = Natural $ fromIntegral $ b - a + 1
    | otherwise = 0
  moebiusInversion x y = case compare x y of
     EQ -> one
     LT | y - 1 == x -> negate one 
     _  -> zero

instance LocallyFiniteOrder Word16 where
  range = curry Ix.range
  rangeSize a b
    | a <= b = Natural $ fromIntegral $ b - a + 1
    | otherwise = 0
  moebiusInversion x y = case compare x y of
     EQ -> one
     LT | y - 1 == x -> negate one 
     _  -> zero

instance LocallyFiniteOrder Word32 where
  range = curry Ix.range
  rangeSize a b
    | a <= b = Natural $ fromIntegral $ b - a + 1
    | otherwise = 0
  moebiusInversion x y = case compare x y of
     EQ -> one
     LT | y - 1 == x -> negate one 
     _  -> zero

instance LocallyFiniteOrder Word64 where
  range = curry Ix.range
  rangeSize a b
    | a <= b = Natural $ fromIntegral $ b - a + 1
    | otherwise = 0
  moebiusInversion x y = case compare x y of
     EQ -> one
     LT | y - 1 == x -> negate one 
     _  -> zero

instance LocallyFiniteOrder () where
  range _ _ = [()]
  rangeSize _ _ = 1
  moebiusInversion _ _ = one

instance ( LocallyFiniteOrder a
         , LocallyFiniteOrder b
         ) => LocallyFiniteOrder (a,b) where
  range (a,b) (i,j) = (,) <$> range a i <*> range b j
  rangeSize (a,b) (i,j) = rangeSize a i * rangeSize b j
  -- TODO: check this against the default definition above
  moebiusInversion (a,b) (i,j) = moebiusInversion a i * moebiusInversion b j

instance ( LocallyFiniteOrder a
         , LocallyFiniteOrder b
         , LocallyFiniteOrder c
         ) => LocallyFiniteOrder (a,b,c) where
  range (a,b,c) (i,j,k) = (,,) <$> range  a i <*> range b j <*> range c k
  rangeSize (a,b,c) (i,j,k) = rangeSize a i * rangeSize b j * rangeSize c k
  moebiusInversion (a,b,c) (i,j,k) = moebiusInversion a i * moebiusInversion b j * moebiusInversion c k


instance ( LocallyFiniteOrder a
         , LocallyFiniteOrder b
         , LocallyFiniteOrder c
         , LocallyFiniteOrder d
         ) => LocallyFiniteOrder (a,b,c,d) where
  range (a,b,c,d) (i,j,k,l) = (,,,) <$> range  a i <*> range b j <*> range c k <*> range d l
  rangeSize (a,b,c,d) (i,j,k,l) = rangeSize  a i * rangeSize b j * rangeSize c k * rangeSize d l
  moebiusInversion (a,b,c,d) (i,j,k,l) = moebiusInversion a i * moebiusInversion b j * moebiusInversion c k * moebiusInversion d l

instance ( LocallyFiniteOrder a
         , LocallyFiniteOrder b
         , LocallyFiniteOrder c
         , LocallyFiniteOrder d
         , LocallyFiniteOrder e
         ) => LocallyFiniteOrder (a, b, c, d, e) where
  range (a,b,c,d,e) (i,j,k,l,m) = (,,,,) <$> range  a i <*> range b j <*> range c k <*> range d l <*> range e m
  rangeSize (a,b,c,d,e) (i,j,k,l,m) = rangeSize  a i * rangeSize b j * rangeSize c k * rangeSize d l * rangeSize e m
  moebiusInversion (a,b,c,d,e) (i,j,k,l,m) = moebiusInversion a i * moebiusInversion b j * moebiusInversion c k * moebiusInversion d l * moebiusInversion e m

