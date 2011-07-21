{-# LANGUAGE DeriveDataTypeable
module Numeric.Ring.Monoid
  ( Mon(..)
  ) where

import Data.Data
import Data.Ix
import Numeric.Algebra

-- basis for the Hopf group ring algebra 
newtype GroupBasis m = GroupBasis m deriving 
  ( Eq, Ord, Show, Read, Ix, Enum, Bounded, Num, Real, Integral
  , Additive, Abelian, Idempotent, Partitionable
  , LeftModule Natural, RightModule Natural, Monoidal
  , LeftModule Integer, RightModule Integer, Group
  , Data, Typeable
  ) where

runMon :: GroupBasis m -> m
runMon (Mon m) = m

instance HasTrie m => HasTrie (GroupBasis m) where
  type BaseTrie (Mon m) = BaseTrie m
  embedKey = embedKey . runMon
  projectKey = Mon . projectKey

instance (Monoidal r, Semiring r, Eq m, Additive m) => Coalgebra r (GroupBasis m) where
  comult f m n | m == n    = f m
               | otherwise = zero

instance (Monoidal r, Semiring r, Eq m, Monoidal m) => CounitalCoalgebra r (GroupBasis m) where
  counit f = f zero
  
instance (Commutative r, Semiring r, Eq m, Abelian m) => CocommutativeCoalgebra r (GroupBasis m)
  
-- TODO: check
instance (Commutative r, IdempotentSemiring r, Eq m, Idempotent m) => IdempotentCoalgebra r (GroupBasis m) 

instance (Semiring r, Partitionable m) => Algebra r (Mon m) where
  mult f = sum1 . partitionWith f

instance (Monoidal r, Semiring r, Partitionable m, DecidableZero m) => UnitalAlgebra r (GroupBasis m) where
  unit x m | isZero m  = x
           | otherwise = zero

instance (Commutative r, Semiring r, Partitionable m, Abelian m) => CommutativeCoalgebra r (GroupBasis m)

instance (IdempotentSemiring r, Partitionable m, Idempotent m) => IdempotentAlgebra r (GroupBasis m)

instance (Monoidal r, Semiring r, Eq m, Partitionable m, DecidableZero m) => Bialgebra r (GroupBasis m) where

instance (Group r, Semiring r, Eq m, Partitionable m, DecidableZero m) => HopfAlgebra r (GroupBasis m) where
  antipode f m = f (negate m)
