{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , TypeFamilies
           , UndecidableInstances
           , DeriveDataTypeable
           , TypeOperators #-}
module Numeric.Algebra.Complex
  ( Distinguished(..)
  , Complicated(..)
  , ComplexBasis(..)
  , Complex(..)
  , realPart
  , imagPart
  , uncomplicate
  ) where

import Control.Applicative
import Control.Monad.Reader.Class
import Data.Data
import Data.Distributive
import Data.Functor.Bind
import Data.Functor.Rep
import Data.Foldable
import Data.Ix hiding (index)
import Data.Semigroup
import Data.Semigroup.Traversable
import Data.Semigroup.Foldable
import Data.Traversable
import Numeric.Algebra
import Numeric.Algebra.Distinguished.Class
import Numeric.Algebra.Complex.Class
import Numeric.Algebra.Quaternion.Class
import Prelude hiding ((-),(+),(*),negate,subtract, fromInteger,recip)

-- complex basis
data ComplexBasis = E | I deriving (Eq,Ord,Show,Read,Enum,Ix,Bounded,Data,Typeable)
data Complex a = Complex a a deriving (Eq,Show,Read,Data,Typeable)

realPart :: (Representable f, Rep f ~ ComplexBasis) => f a -> a
realPart f = index f E 

imagPart :: (Representable f, Rep f ~ ComplexBasis) => f a -> a
imagPart f = index f I

instance Distinguished ComplexBasis where
  e = E
  
instance Complicated ComplexBasis where
  i = I

instance Rig r => Distinguished (Complex r) where
  e = Complex one zero

instance Rig r => Complicated (Complex r) where
  i = Complex zero one

instance Rig r => Distinguished (ComplexBasis -> r) where
  e E = one
  e _ = zero
  
instance Rig r => Complicated (ComplexBasis -> r) where
  i I = one
  i _ = zero 

instance Representable Complex where
  type Rep Complex = ComplexBasis
  tabulate f = Complex (f E) (f I)
  index (Complex a _ ) E = a
  index (Complex _ b ) I = b

instance Distributive Complex where
  distribute = distributeRep 

instance Functor Complex where
  fmap f (Complex a b) = Complex (f a) (f b)

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

instance Traversable Complex where
  traverse f (Complex a b) = Complex <$> f a <*> f b

instance Foldable1 Complex where
  foldMap1 f (Complex a b) = f a <> f b

instance Traversable1 Complex where
  traverse1 f (Complex a b) = Complex <$> f a <.> f b

instance Additive r => Additive (Complex r) where
  (+) = addRep 
  sinnum1p = sinnum1pRep

instance LeftModule r s => LeftModule r (Complex s) where
  r .* Complex a b = Complex (r .* a) (r .* b)

instance RightModule r s => RightModule r (Complex s) where
  Complex a b *. r = Complex (a *. r) (b *. r)

instance Monoidal r => Monoidal (Complex r) where
  zero = zeroRep
  sinnum = sinnumRep

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

-- the trivial coalgebra
instance Rng k => Coalgebra k ComplexBasis where
  comult f E E = f E
  comult f I I = f I
  comult _ _ _ = zero

instance Rng k => CounitalCoalgebra k ComplexBasis where
  counit f = f E + f I

instance Rng k => Bialgebra k ComplexBasis 

instance (InvolutiveSemiring k, Rng k) => InvolutiveAlgebra k ComplexBasis where
  inv f = f' where
    afe = adjoint (f E)
    nfi = negate (f I)
    f' E = afe
    f' I = nfi

instance (InvolutiveSemiring k, Rng k) => InvolutiveCoalgebra k ComplexBasis where
  coinv = inv

instance (InvolutiveSemiring k, Rng k) => HopfAlgebra k ComplexBasis where
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

instance {-# OVERLAPPING #-} (Commutative r, Rng r) => LeftModule (Complex r) (Complex r) where
  (.*) = (*)
instance {-# OVERLAPPING #-} (Commutative r, Rng r) => RightModule (Complex r) (Complex r) where
  (*.) = (*)

instance (Commutative r, Rng r, InvolutiveMultiplication r) => InvolutiveMultiplication (Complex r) where
  adjoint (Complex a b) = Complex (adjoint a) (negate b)

instance (Commutative r, Rng r, InvolutiveSemiring r) => InvolutiveSemiring (Complex r)

instance (Commutative r, Rng r, InvolutiveSemiring r) => Quadrance r (Complex r) where
  quadrance n = realPart $ adjoint n * n

instance (Commutative r, InvolutiveSemiring r, DivisionRing r) => Division (Complex r) where
  recip q@(Complex a b) = Complex (qq \\ a) (qq \\ b)
    where qq = quadrance q

-- | half of the Cayley-Dickson quaternion isomorphism 
uncomplicate :: Hamiltonian q => ComplexBasis -> ComplexBasis -> q
uncomplicate E E = e
uncomplicate I E = i
uncomplicate E I = j
uncomplicate I I = k

