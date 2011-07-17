{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
module Numeric.Quaternion 
  ( Hamiltonian(..)
  , QuaternionBasis(..)
  , Quaternion(..)
  , complicate
  , uncomplicate
  ) where

import Control.Applicative
import Control.Monad.Reader.Class
import Data.Ix
import Data.Key
import Data.Distributive
import Data.Functor.Bind
import Data.Functor.Representable
import Data.Functor.Representable.Trie
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Data.Semigroup.Traversable
import Data.Semigroup.Foldable
import Numeric.Covector
import Numeric.Addition
import Numeric.Algebra
import Numeric.Group 
import Numeric.Module
import Numeric.Multiplication
import Numeric.Semiring
import Numeric.Rig
import Numeric.Rng
import Numeric.Ring
import Numeric.Complex (ComplexBasis)
import qualified Numeric.Complex as Complex
import Prelude hiding ((-),(+),(*),negate,subtract, fromInteger)

{-
ifThenElse :: Bool -> a -> a -> a
ifThenElse True a _ = a
ifThenElse False _ b = b
-}

class Hamiltonian t where
  e :: t
  i :: t
  j :: t
  k :: t

instance Hamiltonian QuaternionBasis where
  e = E
  i = I
  j = J
  k = K

instance Rig r => Hamiltonian (Quaternion r) where
  e = Quaternion one zero zero zero
  i = Quaternion zero one zero zero
  j = Quaternion zero zero one zero
  k = Quaternion one zero zero one 

instance Rig r => Hamiltonian (QuaternionBasis -> r) where
  e E = one 
  e _ = zero
  i I = one
  i _ = zero
  j J = one
  j _ = zero
  k K = one
  k _ = zero

instance Hamiltonian a => Hamiltonian (Covector r a) where
  e = return e
  i = return i
  j = return j
  k = return k

-- quaternion basis
data QuaternionBasis = E | I | J | K deriving (Eq,Ord,Enum,Show,Bounded,Ix)

data Quaternion a = Quaternion a a a a 

type instance Key Quaternion = QuaternionBasis

instance Representable Quaternion where
  tabulate f = Quaternion (f E) (f I) (f J) (f K)

instance Indexable Quaternion where
  index (Quaternion a _ _ _) E = a
  index (Quaternion _ b _ _) I = b
  index (Quaternion _ _ c _) J = c
  index (Quaternion _ _ _ d) K = d

instance Lookup Quaternion where
  lookup = lookupDefault

instance Adjustable Quaternion where
  adjust f E (Quaternion a b c d) = Quaternion (f a) b c d
  adjust f I (Quaternion a b c d) = Quaternion a (f b) c d
  adjust f J (Quaternion a b c d) = Quaternion a b (f c) d
  adjust f K (Quaternion a b c d) = Quaternion a b c (f d)

instance Distributive Quaternion where
  distribute = distributeRep 

instance Functor Quaternion where
  fmap = fmapRep

instance Zip Quaternion where
  zipWith f (Quaternion a1 b1 c1 d1) (Quaternion a2 b2 c2 d2) = Quaternion (f a1 a2) (f b1 b2) (f c1 c2) (f d1 d2)

instance ZipWithKey Quaternion where
  zipWithKey f (Quaternion a1 b1 c1 d1) (Quaternion a2 b2 c2 d2) = Quaternion (f E a1 a2) (f I b1 b2) (f J c1 c2) (f K d1 d2)

instance Keyed Quaternion where
  mapWithKey = mapWithKeyRep

instance Apply Quaternion where
  (<.>) = apRep

instance Applicative Quaternion where
  pure = pureRep
  (<*>) = apRep 

instance Bind Quaternion where
  (>>-) = bindRep

instance Monad Quaternion where
  return = pureRep
  (>>=) = bindRep

instance MonadReader QuaternionBasis Quaternion where
  ask = askRep
  local = localRep

instance Foldable Quaternion where
  foldMap f (Quaternion a b c d) = f a `mappend` f b `mappend` f c `mappend` f d

instance FoldableWithKey Quaternion where
  foldMapWithKey f (Quaternion a b c d) = f E a `mappend` f I b `mappend` f J c `mappend` f K d

instance Traversable Quaternion where
  traverse f (Quaternion a b c d) = Quaternion <$> f a <*> f b <*> f c <*> f d

instance TraversableWithKey Quaternion where
  traverseWithKey f (Quaternion a b c d) = Quaternion <$> f E a <*> f I b <*> f J c <*> f K d

instance Foldable1 Quaternion where
  foldMap1 f (Quaternion a b c d) = f a <> f b <> f c <> f d

instance FoldableWithKey1 Quaternion where
  foldMapWithKey1 f (Quaternion a b c d) = f E a <> f I b <> f J c <> f K d

instance Traversable1 Quaternion where
  traverse1 f (Quaternion a b c d) = Quaternion <$> f a <.> f b <.> f c <.> f d

instance TraversableWithKey1 Quaternion where
  traverseWithKey1 f (Quaternion a b c d) = Quaternion <$> f E a <.> f I b <.> f J c <.> f K d

instance HasTrie QuaternionBasis where
  type BaseTrie QuaternionBasis = Quaternion
  embedKey = id
  projectKey = id

instance Additive r => Additive (Quaternion r) where
  (+) = addRep 
  replicate1p = replicate1pRep

instance LeftModule r s => LeftModule r (Quaternion s) where
  r .* Quaternion a b c d = Quaternion (r .* a) (r .* b) (r .* c) (r .* d)

instance RightModule r s => RightModule r (Quaternion s) where
  Quaternion a b c d *. r = Quaternion (a *. r) (b *. r) (c *. r) (d *. r)

instance Monoidal r => Monoidal (Quaternion r) where
  zero = zeroRep
  replicate = replicateRep

instance Group r => Group (Quaternion r) where
  (-) = minusRep
  negate = negateRep
  subtract = subtractRep
  times = timesRep

instance Abelian r => Abelian (Quaternion r)

instance Idempotent r => Idempotent (Quaternion r)

instance Partitionable r => Partitionable (Quaternion r) where
  partitionWith f (Quaternion a b c d) = id =<<
    partitionWith (\a1 a2 -> id =<< 
    partitionWith (\b1 b2 -> id =<< 
    partitionWith (\c1 c2 -> 
    partitionWith (\d1 d2 -> f (Quaternion a1 b1 c1 d1) (Quaternion a2 b2 c2 d2)) d) c) b) a


instance Rng k => Algebra k QuaternionBasis where
  mult f = f' where
    f' E = f E E - (f I I + f J J + f K K)
    f' I = f E I + f I E + f J K - f K J
    f' J = f E J + f J E + f K I - f I K
    f' K = f E K + f K E + f I J - f J I

instance Rng k => UnitalAlgebra k QuaternionBasis where
  unit x E = x 
  unit _ _ = zero

instance Rng k => Coalgebra k QuaternionBasis where
  comult f E b = f b
  comult f b E = f b 
  comult f I I = negate (f E)
  comult f I J = f K
  comult f I K = negate (f J)
  comult f J J = negate (f E)
  comult f J I = negate (f K)
  comult f J K = f I
  comult f K I = f J
  comult f K K = negate (f E)
  comult f K J = negate (f I)

instance Rng k => CounitalCoalgebra k QuaternionBasis where
  counit f = f E

instance Rng k => Bialgebra k QuaternionBasis 

instance Rng k => InvolutiveAlgebra k QuaternionBasis where
  inv f E = f E
  inv f b = negate (f b)

instance Rng k => InvolutiveCoalgebra k QuaternionBasis where
  coinv = inv

instance Rng k => HopfAlgebra k QuaternionBasis where
  antipode = inv

instance Rng r => Multiplicative (Quaternion r) where
  (*) = mulRep

instance Rng r => Semiring (Quaternion r)

instance Ring r => Unital (Quaternion r) where
  one = oneRep

instance Ring r => Rig (Quaternion r) where
  fromNatural n = Quaternion (fromNatural n) zero zero zero

instance Ring r => Ring (Quaternion r) where
  fromInteger n = Quaternion (fromInteger n) zero zero zero

instance Rng r => LeftModule (Quaternion r) (Quaternion r) where (.*) = (*)
instance Rng r => RightModule (Quaternion r) (Quaternion r) where (*.) = (*)

instance (Rng r, InvolutiveMultiplication r) => InvolutiveMultiplication (Quaternion r) where
  -- TODO: check this against the Albert's generalized Cayley-Dickson construction
  adjoint (Quaternion a b c d) = Quaternion (adjoint a) (negate (adjoint b)) (negate (adjoint c)) (adjoint d)

-- | Cayley-Dickson quaternion isomorphism (one way)
complicate :: QuaternionBasis -> (ComplexBasis, ComplexBasis)
complicate E = (Complex.E, Complex.E)
complicate I = (Complex.I, Complex.E)
complicate J = (Complex.E, Complex.I)
complicate K = (Complex.I, Complex.I)

-- | Cayley-Dickson quaternion isomorphism (the other half)
uncomplicate :: ComplexBasis -> ComplexBasis -> QuaternionBasis
uncomplicate Complex.E Complex.E = E
uncomplicate Complex.I Complex.E = I
uncomplicate Complex.E Complex.I = J
uncomplicate Complex.I Complex.I = K
