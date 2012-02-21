{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, UndecidableInstances, DeriveDataTypeable #-}
module Numeric.Coalgebra.Hyperbolic 
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
import Data.Semigroup.Traversable
import Data.Semigroup.Foldable
import Data.Semigroup
import Data.Traversable
import Numeric.Algebra
import Numeric.Coalgebra.Hyperbolic.Class
import Prelude hiding ((-),(+),(*),negate,subtract, fromInteger, cosh, sinh)

-- complex basis
data HyperBasis = Cosh | Sinh deriving (Eq,Ord,Show,Read,Enum,Ix,Bounded,Data,Typeable)
data Hyper a = Hyper a a deriving (Eq,Show,Read,Data,Typeable)

instance Hyperbolic HyperBasis where
  cosh = Cosh
  sinh = Sinh

instance Rig r => Hyperbolic (Hyper r) where
  cosh = Hyper one zero
  sinh = Hyper zero one
  
instance Rig r => Hyperbolic (HyperBasis -> r) where
  cosh Sinh = zero
  cosh Cosh = one
  sinh Sinh = one
  sinh Cosh = zero

type instance Key Hyper = HyperBasis

instance Representable Hyper where
  tabulate f = Hyper (f Cosh) (f Sinh)

instance Indexable Hyper where
  index (Hyper a _ ) Cosh = a
  index (Hyper _ b ) Sinh = b

instance Lookup Hyper where
  lookup = lookupDefault

instance Adjustable Hyper where
  adjust f Cosh (Hyper a b) = Hyper (f a) b
  adjust f Sinh (Hyper a b) = Hyper a (f b)

instance Distributive Hyper where
  distribute = distributeRep 

instance Functor Hyper where
  fmap f (Hyper a b) = Hyper (f a) (f b)

instance Zip Hyper where
  zipWith f (Hyper a1 b1) (Hyper a2 b2) = Hyper (f a1 a2) (f b1 b2)

instance ZipWithKey Hyper where
  zipWithKey f (Hyper a1 b1) (Hyper a2 b2) = Hyper (f Cosh a1 a2) (f Sinh b1 b2)

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
  foldMapWithKey f (Hyper a b) = f Cosh a `mappend` f Sinh b

instance Traversable Hyper where
  traverse f (Hyper a b) = Hyper <$> f a <*> f b

instance TraversableWithKey Hyper where
  traverseWithKey f (Hyper a b) = Hyper <$> f Cosh a <*> f Sinh b

instance Foldable1 Hyper where
  foldMap1 f (Hyper a b) = f a <> f b

instance FoldableWithKey1 Hyper where
  foldMapWithKey1 f (Hyper a b) = f Cosh a <> f Sinh b

instance Traversable1 Hyper where
  traverse1 f (Hyper a b) = Hyper <$> f a <.> f b

instance TraversableWithKey1 Hyper where
  traverseWithKey1 f (Hyper a b) = Hyper <$> f Cosh a <.> f Sinh b

instance HasTrie HyperBasis where
  type BaseTrie HyperBasis = Hyper
  embedKey = id
  projectKey = id

instance Additive r => Additive (Hyper r) where
  (+) = addRep 
  sinnum1p = sinnum1pRep

instance LeftModule r s => LeftModule r (Hyper s) where
  r .* Hyper a b = Hyper (r .* a) (r .* b)

instance RightModule r s => RightModule r (Hyper s) where
  Hyper a b *. r = Hyper (a *. r) (b *. r)

instance Monoidal r => Monoidal (Hyper r) where
  zero = zeroRep
  sinnum = sinnumRep

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

-- | the trivial diagonal algebra
instance Semiring k => Algebra k HyperBasis where
  mult f = f' where
    fs = f Sinh Sinh
    fc = f Cosh Cosh
    f' Sinh = fs
    f' Cosh = fc

instance Semiring k => UnitalAlgebra k HyperBasis where
  unit = const

-- | the hyperbolic trigonometric coalgebra
instance (Commutative k, Semiring k) => Coalgebra k HyperBasis where
  comult f = f' where
     fs = f Sinh
     fc = f Cosh
     f' Sinh Sinh = fc
     f' Sinh Cosh = fs 
     f' Cosh Sinh = fs
     f' Cosh Cosh = fc

instance (Commutative k, Semiring k) => CounitalCoalgebra k HyperBasis where
  counit f = f Cosh

instance (Commutative k, Semiring k) => Bialgebra k HyperBasis

instance (Commutative k, Group k, InvolutiveSemiring k) => InvolutiveAlgebra k HyperBasis where
  inv f = f' where
    afc = adjoint (f Cosh)
    nfs = negate (f Sinh)
    f' Cosh = afc
    f' Sinh = nfs

instance (Commutative k, Group k, InvolutiveSemiring k) => InvolutiveCoalgebra k HyperBasis where
  coinv = inv

instance (Commutative k, Group k, InvolutiveSemiring k) => HopfAlgebra k HyperBasis where
  antipode = inv

instance (Commutative k, Semiring k) => Multiplicative (Hyper k) where
  (*) = mulRep

instance (Commutative k, Semiring k) => Commutative (Hyper k)

instance (Commutative k, Semiring k) => Semiring (Hyper k)

instance (Commutative k, Rig k) => Unital (Hyper k) where
  one = Hyper one zero

instance (Commutative r, Rig r) => Rig (Hyper r) where
  fromNatural n = Hyper (fromNatural n) zero

instance (Commutative r, Ring r) => Ring (Hyper r) where
  fromInteger n = Hyper (fromInteger n) zero

instance (Commutative r, Semiring r) => LeftModule (Hyper r) (Hyper r) where (.*) = (*)
instance (Commutative r, Semiring r) => RightModule (Hyper r) (Hyper r) where (*.) = (*)

instance (Commutative r, Group r, InvolutiveSemiring r) => InvolutiveMultiplication (Hyper r) where
  adjoint (Hyper a b) = Hyper (adjoint a) (negate b)

instance (Commutative r, Group r, InvolutiveSemiring r) => InvolutiveSemiring (Hyper r)
