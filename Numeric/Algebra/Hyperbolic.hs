{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, UndecidableInstances, DeriveDataTypeable #-}
module Numeric.Algebra.Hyperbolic 
  ( Hyperbolic(..)
  , HyperBasis(..)
  , Hyper(..)
  ) where

import Control.Applicative
import Control.Monad.Reader.Class
import Data.Data
import Data.Distributive
import Data.Functor.Bind
import Data.Functor.Representable
import Data.Functor.Representable.Trie
import Data.Foldable
import Data.Ix
import Data.Key
import Data.Monoid
import Data.Semigroup.Traversable
import Data.Semigroup.Foldable
import Data.Traversable
import Numeric.Algebra
import Prelude hiding ((-),(+),(*),negate,subtract, fromInteger)

-- complex basis
data HyperBasis = S | C deriving (Eq,Ord,Show,Read,Enum,Ix,Bounded,Data,Typeable)
data Hyper a = Hyper a a deriving (Eq,Show,Read,Data,Typeable)

class Hyperbolic r where
  s :: r
  c :: r

instance Hyperbolic HyperBasis where
  s = S
  c = C

instance Rig r => Hyperbolic (Hyper r) where
  s = Hyper one zero
  c = Hyper zero one
  
instance Rig r => Hyperbolic (HyperBasis -> r) where
  s S = one
  s C = zero
  c S = zero
  c C = one

instance Hyperbolic a => Hyperbolic (Covector r a) where
  s = return s
  c = return c

type instance Key Hyper = HyperBasis

instance Representable Hyper where
  tabulate f = Hyper (f S) (f C)

instance Indexable Hyper where
  index (Hyper a _ ) S = a
  index (Hyper _ b ) C = b

instance Lookup Hyper where
  lookup = lookupDefault

instance Adjustable Hyper where
  adjust f S (Hyper a b) = Hyper (f a) b
  adjust f C (Hyper a b) = Hyper a (f b)

instance Distributive Hyper where
  distribute = distributeRep 

instance Functor Hyper where
  fmap f (Hyper a b) = Hyper (f a) (f b)

instance Zip Hyper where
  zipWith f (Hyper a1 b1) (Hyper a2 b2) = Hyper (f a1 a2) (f b1 b2)

instance ZipWithKey Hyper where
  zipWithKey f (Hyper a1 b1) (Hyper a2 b2) = Hyper (f S a1 a2) (f C b1 b2)

instance Keyed Hyper where
  mapWithKey = mapWithKeyRep

instance Apply Hyper where
  (<.>) = apRep

instance Applicative Hyper where
  pure = pureRep
  (<*>) = apRep 

instance Bind Hyper where
  (>>-) = bindRep

instance Monad Hyper where
  return = pureRep
  (>>=) = bindRep

instance MonadReader HyperBasis Hyper where
  ask = askRep
  local = localRep

instance Foldable Hyper where
  foldMap f (Hyper a b) = f a `mappend` f b

instance FoldableWithKey Hyper where
  foldMapWithKey f (Hyper a b) = f S a `mappend` f C b

instance Traversable Hyper where
  traverse f (Hyper a b) = Hyper <$> f a <*> f b

instance TraversableWithKey Hyper where
  traverseWithKey f (Hyper a b) = Hyper <$> f S a <*> f C b

instance Foldable1 Hyper where
  foldMap1 f (Hyper a b) = f a <> f b

instance FoldableWithKey1 Hyper where
  foldMapWithKey1 f (Hyper a b) = f S a <> f C b

instance Traversable1 Hyper where
  traverse1 f (Hyper a b) = Hyper <$> f a <.> f b

instance TraversableWithKey1 Hyper where
  traverseWithKey1 f (Hyper a b) = Hyper <$> f S a <.> f C b

instance HasTrie HyperBasis where
  type BaseTrie HyperBasis = Hyper
  embedKey = id
  projectKey = id

instance Additive r => Additive (Hyper r) where
  (+) = addRep 
  replicate1p = replicate1pRep

instance LeftModule r s => LeftModule r (Hyper s) where
  r .* Hyper a b = Hyper (r .* a) (r .* b)

instance RightModule r s => RightModule r (Hyper s) where
  Hyper a b *. r = Hyper (a *. r) (b *. r)

instance Monoidal r => Monoidal (Hyper r) where
  zero = zeroRep
  replicate = replicateRep

instance Group r => Group (Hyper r) where
  (-) = minusRep
  negate = negateRep
  subtract = subtractRep
  times = timesRep

instance Abelian r => Abelian (Hyper r)

instance Idempotent r => Idempotent (Hyper r)

instance Partitionable r => Partitionable (Hyper r) where
  partitionWith f (Hyper a b) = id =<<
    partitionWith (\a1 a2 -> 
    partitionWith (\b1 b2 -> f (Hyper a1 b1) (Hyper a2 b2)) b) a

-- the dual, hyperbolic trigonometric algebra
instance (Commutative k, Semiring k) => Algebra k HyperBasis where
  mult f = f' where
    fs = f S C + f C S
    fc = f C C + f S S
    f' S = fs
    f' C = fc

instance (Commutative k, Monoidal k, Semiring k) => UnitalAlgebra k HyperBasis where
  unit _ S = zero
  unit x C = x

-- the actual hyperbolic trigonometric coalgebra
instance (Commutative k, Semiring k) => Coalgebra k HyperBasis where
  comult f = f' where
     fs = f S
     fc = f C
     f' S S = fc
     f' S C = fs 
     f' C S = fs
     f' C C = fc

instance (Commutative k, Semiring k) => CounitalCoalgebra k HyperBasis where
  counit f = f C

instance (Commutative k, Semiring k) => Multiplicative (Hyper k) where
  (*) = mulRep

instance (Commutative k, Semiring k) => Commutative (Hyper k)

instance (Commutative k, Semiring k) => Semiring (Hyper k)

instance (Commutative k, Rig k) => Unital (Hyper k) where
  one = Hyper zero one

instance (Commutative r, Rig r) => Rig (Hyper r) where
  fromNatural n = Hyper zero (fromNatural n)

instance (Commutative r, Ring r) => Ring (Hyper r) where
  fromInteger n = Hyper zero (fromInteger n)

instance (Commutative r, Semiring r) => LeftModule (Hyper r) (Hyper r) where (.*) = (*)
instance (Commutative r, Semiring r) => RightModule (Hyper r) (Hyper r) where (*.) = (*)

instance (Commutative r, Semiring r, InvolutiveMultiplication r) => InvolutiveMultiplication (Hyper r) where
  adjoint (Hyper a b) = Hyper (adjoint a) (adjoint b)

instance (Commutative r, InvolutiveSemiring r) => InvolutiveSemiring (Hyper r)
