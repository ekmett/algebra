{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , TypeFamilies
           , UndecidableInstances
           , DeriveDataTypeable
           , TypeOperators #-}
module Numeric.Coalgebra.Trigonometric 
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
import Data.Semigroup.Traversable
import Data.Semigroup.Foldable
import Data.Semigroup
import Data.Traversable
import Numeric.Algebra
import Prelude hiding ((-),(+),(*),negate,subtract, fromInteger, sin, cos)
import Numeric.Algebra.Distinguished.Class
import Numeric.Algebra.Complex.Class
import Numeric.Coalgebra.Trigonometric.Class

-- the dual complex basis
data TrigBasis = Cos | Sin deriving (Eq,Ord,Show,Read,Enum,Ix,Bounded,Data,Typeable)
data Trig a = Trig a a deriving (Eq,Show,Read,Data,Typeable)

instance Distinguished TrigBasis where
  e = Cos

instance Complicated TrigBasis where
  i = Sin

instance Trigonometric TrigBasis where
  cos = Cos
  sin = Sin

instance Rig r => Distinguished (Trig r) where
  e = Trig one zero

instance Rig r => Complicated (Trig r) where
  i = Trig zero one

instance Rig r => Trigonometric (Trig r) where
  cos = Trig one zero
  sin = Trig zero one

instance Rig r => Distinguished (TrigBasis -> r) where
  e = cos

instance Rig r => Complicated (TrigBasis -> r) where
  i = sin
  
instance Rig r => Trigonometric (TrigBasis -> r) where
  cos Sin = zero
  cos Cos = one

  sin Sin = one
  sin Cos = zero

instance Rig r => Trigonometric (TrigBasis :->: r) where
  cos = Trie cos
  sin = Trie sin

instance Rig r => Distinguished (TrigBasis :->: r) where
  e = Trie e

instance Rig r => Complicated (TrigBasis :->: r) where
  i = Trie i
  
type instance Key Trig = TrigBasis

instance Representable Trig where
  tabulate f = Trig (f Cos) (f Sin)

instance Indexable Trig where
  index (Trig a _ ) Cos = a
  index (Trig _ b ) Sin = b

instance Lookup Trig where
  lookup = lookupDefault

instance Adjustable Trig where
  adjust f Cos (Trig a b) = Trig (f a) b
  adjust f Sin (Trig a b) = Trig a (f b)

instance Distributive Trig where
  distribute = distributeRep 

instance Functor Trig where
  fmap f (Trig a b) = Trig (f a) (f b)

instance Zip Trig where
  zipWith f (Trig a1 b1) (Trig a2 b2) = Trig (f a1 a2) (f b1 b2)

instance ZipWithKey Trig where
  zipWithKey f (Trig a1 b1) (Trig a2 b2) = Trig (f Cos a1 a2) (f Sin b1 b2)

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
  foldMapWithKey f (Trig a b) = f Cos a `mappend` f Sin b

instance Traversable Trig where
  traverse f (Trig a b) = Trig <$> f a <*> f b

instance TraversableWithKey Trig where
  traverseWithKey f (Trig a b) = Trig <$> f Cos a <*> f Sin b

instance Foldable1 Trig where
  foldMap1 f (Trig a b) = f a <> f b

instance FoldableWithKey1 Trig where
  foldMapWithKey1 f (Trig a b) = f Cos a <> f Sin b

instance Traversable1 Trig where
  traverse1 f (Trig a b) = Trig <$> f a <.> f b

instance TraversableWithKey1 Trig where
  traverseWithKey1 f (Trig a b) = Trig <$> f Cos a <.> f Sin b

instance HasTrie TrigBasis where
  type BaseTrie TrigBasis = Trig
  embedKey = id
  projectKey = id

instance Additive r => Additive (Trig r) where
  (+) = addRep 
  sinnum1p = sinnum1pRep

instance LeftModule r s => LeftModule r (Trig s) where
  r .* Trig a b = Trig (r .* a) (r .* b)

instance RightModule r s => RightModule r (Trig s) where
  Trig a b *. r = Trig (a *. r) (b *. r)

instance Monoidal r => Monoidal (Trig r) where
  zero = zeroRep
  sinnum = sinnumRep

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

-- the diagonal algebra
instance (Commutative k, Rng k) => Algebra k TrigBasis where
  mult f = f' where
    fc = f Cos Cos
    fs = f Sin Sin
    f' Cos = fc
    f' Sin = fs

-- 
instance (Commutative k, Rng k) => UnitalAlgebra k TrigBasis where
  unit = const

-- The trigonometric coalgebra
instance (Commutative k, Rng k) => Coalgebra k TrigBasis where
  comult f = f' where
     fs = f Sin
     fc = f Cos
     fc' = negate fc
     f' Sin Sin = fc'
     f' Sin Cos = fs 
     f' Cos Sin = fs
     f' Cos Cos = fc

instance (Commutative k, Rng k) => Bialgebra k TrigBasis

instance (Commutative k, Group k, InvolutiveSemiring k) => InvolutiveAlgebra k TrigBasis where
  inv f = f' where
    afc = adjoint (f Cos)
    nfs = negate (f Sin)
    f' Cos = afc
    f' Sin = nfs

instance (Commutative k, Group k, InvolutiveSemiring k) => InvolutiveCoalgebra k TrigBasis where
  coinv = inv

instance (Commutative k, Group k, InvolutiveSemiring k) => HopfAlgebra k TrigBasis where
  antipode = inv

instance (Commutative k, Rng k) => CounitalCoalgebra k TrigBasis where
  counit f = f Cos

instance (Commutative k, Rng k) => Multiplicative (Trig k) where
  (*) = mulRep

instance (Commutative k, Rng k) => Commutative (Trig k)

instance (Commutative k, Rng k) => Semiring (Trig k)

instance (Commutative k, Ring k) => Unital (Trig k) where
  one = Trig one zero

instance (Commutative r, Ring r) => Rig (Trig r) where
  fromNatural n = Trig (fromNatural n) zero

instance (Commutative r, Ring r) => Ring (Trig r) where
  fromInteger n = Trig (fromInteger n) zero

instance (Commutative r, Rng r) => LeftModule (Trig r) (Trig r) where (.*) = (*)
instance (Commutative r, Rng r) => RightModule (Trig r) (Trig r) where (*.) = (*)

instance (Commutative r, Rng r, InvolutiveMultiplication r) => InvolutiveMultiplication (Trig r) where
  adjoint (Trig a b) = Trig (adjoint a) (negate b)

instance (Commutative r, Rng r, InvolutiveSemiring r) => InvolutiveSemiring (Trig r)
