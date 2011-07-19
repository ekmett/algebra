{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, UndecidableInstances, DeriveDataTypeable #-}
module Numeric.Algebra.Trigonometric 
  ( Trigonometric(..)
  , TrigBasis(..)
  , Trig(..)
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
data TrigBasis = S | C deriving (Eq,Ord,Show,Read,Enum,Ix,Bounded,Data,Typeable)
data Trig a = Trig a a deriving (Eq,Show,Read,Data,Typeable)

class Trigonometric r where
  s :: r
  c :: r

instance Trigonometric TrigBasis where
  s = S
  c = C

instance Rig r => Trigonometric (Trig r) where
  s = Trig one zero
  c = Trig zero one
  
instance Rig r => Trigonometric (TrigBasis -> r) where
  s S = one
  s C = zero
  c S = zero
  c C = one

instance Trigonometric a => Trigonometric (Covector r a) where
  s = return s
  c = return c

type instance Key Trig = TrigBasis

instance Representable Trig where
  tabulate f = Trig (f S) (f C)

instance Indexable Trig where
  index (Trig a _ ) S = a
  index (Trig _ b ) C = b

instance Lookup Trig where
  lookup = lookupDefault

instance Adjustable Trig where
  adjust f S (Trig a b) = Trig (f a) b
  adjust f C (Trig a b) = Trig a (f b)

instance Distributive Trig where
  distribute = distributeRep 

instance Functor Trig where
  fmap f (Trig a b) = Trig (f a) (f b)

instance Zip Trig where
  zipWith f (Trig a1 b1) (Trig a2 b2) = Trig (f a1 a2) (f b1 b2)

instance ZipWithKey Trig where
  zipWithKey f (Trig a1 b1) (Trig a2 b2) = Trig (f S a1 a2) (f C b1 b2)

instance Keyed Trig where
  mapWithKey = mapWithKeyRep

instance Apply Trig where
  (<.>) = apRep

instance Applicative Trig where
  pure = pureRep
  (<*>) = apRep 

instance Bind Trig where
  (>>-) = bindRep

instance Monad Trig where
  return = pureRep
  (>>=) = bindRep

instance MonadReader TrigBasis Trig where
  ask = askRep
  local = localRep

instance Foldable Trig where
  foldMap f (Trig a b) = f a `mappend` f b

instance FoldableWithKey Trig where
  foldMapWithKey f (Trig a b) = f S a `mappend` f C b

instance Traversable Trig where
  traverse f (Trig a b) = Trig <$> f a <*> f b

instance TraversableWithKey Trig where
  traverseWithKey f (Trig a b) = Trig <$> f S a <*> f C b

instance Foldable1 Trig where
  foldMap1 f (Trig a b) = f a <> f b

instance FoldableWithKey1 Trig where
  foldMapWithKey1 f (Trig a b) = f S a <> f C b

instance Traversable1 Trig where
  traverse1 f (Trig a b) = Trig <$> f a <.> f b

instance TraversableWithKey1 Trig where
  traverseWithKey1 f (Trig a b) = Trig <$> f S a <.> f C b

instance HasTrie TrigBasis where
  type BaseTrie TrigBasis = Trig
  embedKey = id
  projectKey = id

instance Additive r => Additive (Trig r) where
  (+) = addRep 
  replicate1p = replicate1pRep

instance LeftModule r s => LeftModule r (Trig s) where
  r .* Trig a b = Trig (r .* a) (r .* b)

instance RightModule r s => RightModule r (Trig s) where
  Trig a b *. r = Trig (a *. r) (b *. r)

instance Monoidal r => Monoidal (Trig r) where
  zero = zeroRep
  replicate = replicateRep

instance Group r => Group (Trig r) where
  (-) = minusRep
  negate = negateRep
  subtract = subtractRep
  times = timesRep

instance Abelian r => Abelian (Trig r)

instance Idempotent r => Idempotent (Trig r)

instance Partitionable r => Partitionable (Trig r) where
  partitionWith f (Trig a b) = id =<<
    partitionWith (\a1 a2 -> 
    partitionWith (\b1 b2 -> f (Trig a1 b1) (Trig a2 b2)) b) a

-- the dual, trigonometric algebra
instance (Commutative k, Rng k) => Algebra k TrigBasis where
  mult f = f' where
    fs = f S C + f C S
    fc = f C C - f S S
    f' S = fs
    f' C = fc

instance (Commutative k, Rng k) => UnitalAlgebra k TrigBasis where
  unit _ S = zero
  unit x C = x

-- the actual trigonometric coalgebra
instance (Commutative k, Rng k) => Coalgebra k TrigBasis where
  comult f = f' where
     fs = f S
     fc = f C
     fc' = negate fc
     f' S S = fc'
     f' S C = fs 
     f' C S = fs
     f' C C = fc

instance (Commutative k, Rng k) => CounitalCoalgebra k TrigBasis where
  counit f = f C

instance (Commutative k, Rng k) => Multiplicative (Trig k) where
  (*) = mulRep

instance (Commutative k, Rng k) => Commutative (Trig k)

instance (Commutative k, Rng k) => Semiring (Trig k)

instance (Commutative k, Ring k) => Unital (Trig k) where
  one = Trig zero one

instance (Commutative r, Ring r) => Rig (Trig r) where
  fromNatural n = Trig zero (fromNatural n)

instance (Commutative r, Ring r) => Ring (Trig r) where
  fromInteger n = Trig zero (fromInteger n)

instance (Commutative r, Rng r) => LeftModule (Trig r) (Trig r) where (.*) = (*)
instance (Commutative r, Rng r) => RightModule (Trig r) (Trig r) where (*.) = (*)

instance (Commutative r, Rng r, InvolutiveMultiplication r) => InvolutiveMultiplication (Trig r) where
  adjoint (Trig a b) = Trig (adjoint a) (adjoint b)

instance (Commutative r, Rng r, InvolutiveSemiring r) => InvolutiveSemiring (Trig r)
