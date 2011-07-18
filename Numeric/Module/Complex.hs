{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, UndecidableInstances, DeriveDataTypeable #-}
module Numeric.Module.Complex
  ( Complicated(..)
  , ComplexBasis(..)
  , Complex
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
data ComplexBasis = E | I deriving (Eq,Ord,Show,Read,Enum,Ix,Bounded,Data,Typeable)
data Complex a = Complex a a deriving (Eq,Show,Read,Data,Typeable)

class Complicated r where
  e :: r
  i :: r

instance Complicated ComplexBasis where
  e = E
  i = I

instance Rig r => Complicated (Complex r) where
  e = Complex one zero
  i = Complex zero one
  
instance Rig r => Complicated (ComplexBasis -> r) where
  e E = one
  e _ = zero
  i I = one
  i _ = zero 

instance Complicated a => Complicated (Covector r a) where
  e = return e
  i = return i

type instance Key Complex = ComplexBasis

instance Representable Complex where
  tabulate f = Complex (f E) (f I)

instance Indexable Complex where
  index (Complex a _ ) E = a
  index (Complex _ b ) I = b

instance Lookup Complex where
  lookup = lookupDefault

instance Adjustable Complex where
  adjust f E (Complex a b) = Complex (f a) b
  adjust f I (Complex a b) = Complex a (f b)

instance Distributive Complex where
  distribute = distributeRep 

instance Functor Complex where
  fmap f (Complex a b) = Complex (f a) (f b)

instance Zip Complex where
  zipWith f (Complex a1 b1) (Complex a2 b2) = Complex (f a1 a2) (f b1 b2)

instance ZipWithKey Complex where
  zipWithKey f (Complex a1 b1) (Complex a2 b2) = Complex (f E a1 a2) (f I b1 b2)

instance Keyed Complex where
  mapWithKey = mapWithKeyRep

instance Apply Complex where
  (<.>) = apRep

instance Applicative Complex where
  pure = pureRep
  (<*>) = apRep 

instance Bind Complex where
  (>>-) = bindRep

instance Monad Complex where
  return = pureRep
  (>>=) = bindRep

instance MonadReader ComplexBasis Complex where
  ask = askRep
  local = localRep

instance Foldable Complex where
  foldMap f (Complex a b) = f a `mappend` f b

instance FoldableWithKey Complex where
  foldMapWithKey f (Complex a b) = f E a `mappend` f I b

instance Traversable Complex where
  traverse f (Complex a b) = Complex <$> f a <*> f b

instance TraversableWithKey Complex where
  traverseWithKey f (Complex a b) = Complex <$> f E a <*> f I b

instance Foldable1 Complex where
  foldMap1 f (Complex a b) = f a <> f b

instance FoldableWithKey1 Complex where
  foldMapWithKey1 f (Complex a b) = f E a <> f I b

instance Traversable1 Complex where
  traverse1 f (Complex a b) = Complex <$> f a <.> f b

instance TraversableWithKey1 Complex where
  traverseWithKey1 f (Complex a b) = Complex <$> f E a <.> f I b

instance HasTrie ComplexBasis where
  type BaseTrie ComplexBasis = Complex
  embedKey = id
  projectKey = id

instance Additive r => Additive (Complex r) where
  (+) = addRep 
  replicate1p = replicate1pRep

instance LeftModule r s => LeftModule r (Complex s) where
  r .* Complex a b = Complex (r .* a) (r .* b)

instance RightModule r s => RightModule r (Complex s) where
  Complex a b *. r = Complex (a *. r) (b *. r)

instance Monoidal r => Monoidal (Complex r) where
  zero = zeroRep
  replicate = replicateRep

instance Group r => Group (Complex r) where
  (-) = minusRep
  negate = negateRep
  subtract = subtractRep
  times = timesRep

instance Abelian r => Abelian (Complex r)

instance Idempotent r => Idempotent (Complex r)

instance Partitionable r => Partitionable (Complex r) where
  partitionWith f (Complex a b) = id =<<
    partitionWith (\a1 a2 -> 
    partitionWith (\b1 b2 -> f (Complex a1 b1) (Complex a2 b2)) b) a

instance Rng k => Algebra k ComplexBasis where
  mult f = f' where
    fe = f E E - f I I
    fi = f E I + f I E
    f' E = fe
    f' I = fi

instance Rng k => UnitalAlgebra k ComplexBasis where
  unit x E = x
  unit _ _ = zero

instance Rng k => Coalgebra k ComplexBasis where
  comult f = f' where 
    fe = f E
    fi = f I
    f' E E = fe
    f' E I = fi
    f' I E = fi
    f' I I = negate fe

instance Rng k => CounitalCoalgebra k ComplexBasis where
  counit f = f E

instance Rng k => Bialgebra k ComplexBasis 

instance Rng k => InvolutiveAlgebra k ComplexBasis where
  inv f E = f E
  inv f b = negate (f b)

instance Rng k => InvolutiveCoalgebra k ComplexBasis where
  coinv = inv

instance Rng k => HopfAlgebra k ComplexBasis where
  antipode = inv

instance (Commutative r, Rng r) => Multiplicative (Complex r) where
  (*) = mulRep

instance (TriviallyInvolutive r, Rng r) => Commutative (Complex r)

instance (Commutative r, Rng r) => Semiring (Complex r)

instance (Commutative r, Ring r) => Unital (Complex r) where
  one = oneRep

instance (Commutative r, Ring r) => Rig (Complex r) where
  fromNatural n = Complex (fromNatural n) zero

instance (Commutative r, Ring r) => Ring (Complex r) where
  fromInteger n = Complex (fromInteger n) zero

instance (Commutative r, Rng r) => LeftModule (Complex r) (Complex r) where (.*) = (*)
instance (Commutative r, Rng r) => RightModule (Complex r) (Complex r) where (*.) = (*)

instance (Commutative r, Rng r, InvolutiveMultiplication r) => InvolutiveMultiplication (Complex r) where
  adjoint (Complex a b) = Complex (adjoint a) (negate b)

instance (Commutative r, Rng r, InvolutiveSemiring r) => InvolutiveSemiring (Complex r)
