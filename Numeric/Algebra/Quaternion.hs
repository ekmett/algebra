{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
           , TypeFamilies
           , UndecidableInstances
           , DeriveDataTypeable
           , TypeOperators #-}
module Numeric.Algebra.Quaternion 
  ( Distinguished(..)
  , Complicated(..)
  , Hamiltonian(..)
  , QuaternionBasis(..)
  , Quaternion(..)
  , complicate
  , vectorPart
  , scalarPart
  ) where

import Control.Applicative
import Control.Monad.Reader.Class
import Data.Ix hiding (index)
import Data.Key
import Data.Data
import Data.Distributive
import Data.Functor.Bind
import Data.Functor.Representable
import Data.Functor.Representable.Trie
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Data.Semigroup.Traversable
import Data.Semigroup.Foldable
import Numeric.Algebra
import Numeric.Algebra.Distinguished.Class
import Numeric.Algebra.Complex.Class
import Numeric.Algebra.Quaternion.Class
import qualified Numeric.Algebra.Complex as Complex
import Prelude hiding ((-),(+),(*),negate,subtract, fromInteger)

instance Distinguished QuaternionBasis where
  e = E

instance Complicated QuaternionBasis where
  i = I

instance Hamiltonian QuaternionBasis where
  j = J
  k = K

instance Rig r => Distinguished (Quaternion r) where
  e = Quaternion one zero zero zero

instance Rig r => Complicated (Quaternion r) where
  i = Quaternion zero one zero zero

instance Rig r => Hamiltonian (Quaternion r) where
  j = Quaternion zero zero one zero
  k = Quaternion one zero zero one 

instance Rig r => Distinguished (QuaternionBasis :->: r) where
  e = Trie e

instance Rig r => Complicated (QuaternionBasis :->: r) where
  i = Trie i

instance Rig r => Hamiltonian (QuaternionBasis :->: r) where
  j = Trie j
  k = Trie k

instance Rig r => Distinguished (QuaternionBasis -> r) where
  e E = one 
  e _ = zero

instance Rig r => Complicated (QuaternionBasis -> r) where
  i I = one
  i _ = zero
  
instance Rig r => Hamiltonian (QuaternionBasis -> r) where
  j J = one
  j _ = zero

  k K = one
  k _ = zero

-- quaternion basis
data QuaternionBasis = E | I | J | K deriving (Eq,Ord,Enum,Read,Show,Bounded,Ix,Data,Typeable)

data Quaternion a = Quaternion a a a a deriving (Eq,Show,Read,Data,Typeable)

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
  zipWith f (Quaternion a1 b1 c1 d1) (Quaternion a2 b2 c2 d2) = 
    Quaternion (f a1 a2) (f b1 b2) (f c1 c2) (f d1 d2)

instance ZipWithKey Quaternion where
  zipWithKey f (Quaternion a1 b1 c1 d1) (Quaternion a2 b2 c2 d2) = 
    Quaternion (f E a1 a2) (f I b1 b2) (f J c1 c2) (f K d1 d2)

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
  foldMap f (Quaternion a b c d) = 
    f a `mappend` f b `mappend` f c `mappend` f d

instance FoldableWithKey Quaternion where
  foldMapWithKey f (Quaternion a b c d) = 
    f E a `mappend` f I b `mappend` f J c `mappend` f K d

instance Traversable Quaternion where
  traverse f (Quaternion a b c d) = 
    Quaternion <$> f a <*> f b <*> f c <*> f d

instance TraversableWithKey Quaternion where
  traverseWithKey f (Quaternion a b c d) = 
    Quaternion <$> f E a <*> f I b <*> f J c <*> f K d

instance Foldable1 Quaternion where
  foldMap1 f (Quaternion a b c d) = 
    f a <> f b <> f c <> f d

instance FoldableWithKey1 Quaternion where
  foldMapWithKey1 f (Quaternion a b c d) = 
    f E a <> f I b <> f J c <> f K d

instance Traversable1 Quaternion where
  traverse1 f (Quaternion a b c d) = 
    Quaternion <$> f a <.> f b <.> f c <.> f d

instance TraversableWithKey1 Quaternion where
  traverseWithKey1 f (Quaternion a b c d) = 
    Quaternion <$> f E a <.> f I b <.> f J c <.> f K d

instance HasTrie QuaternionBasis where
  type BaseTrie QuaternionBasis = Quaternion
  embedKey = id
  projectKey = id

instance Additive r => Additive (Quaternion r) where
  (+) = addRep 
  sinnum1p = sinnum1pRep

instance LeftModule r s => LeftModule r (Quaternion s) where
  r .* Quaternion a b c d =
    Quaternion (r .* a) (r .* b) (r .* c) (r .* d)

instance RightModule r s => RightModule r (Quaternion s) where
  Quaternion a b c d *. r =
    Quaternion (a *. r) (b *. r) (c *. r) (d *. r)

instance Monoidal r => Monoidal (Quaternion r) where
  zero = zeroRep
  sinnum = sinnumRep

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
    partitionWith (\d1 d2 -> f (Quaternion a1 b1 c1 d1) 
                               (Quaternion a2 b2 c2 d2)
                  ) d) c) b) a

-- | the quaternion algebra
instance (TriviallyInvolutive r, Rng r) => Algebra r QuaternionBasis where
  mult f = f' where
    fe = f E E - (f I I + f J J + f K K)
    fi = f E I + f I E + f J K - f K J
    fj = f E J + f J E + f K I - f I K
    fk = f E K + f K E + f I J - f J I
    f' E = fe
    f' I = fi
    f' J = fj
    f' K = fk
             
instance (TriviallyInvolutive r, Rng r) => UnitalAlgebra r QuaternionBasis where
  unit x E = x 
  unit _ _ = zero

-- | the trivial diagonal coalgebra
instance (TriviallyInvolutive r, Rng r) => Coalgebra r QuaternionBasis where
  comult f = f' where
    fe = f E
    fi = f I
    fj = f J
    fk = f K
    f' E E = fe
    f' I I = fi
    f' J J = fj
    f' K K = fk
    f' _ _ = zero

instance (TriviallyInvolutive r, Rng r) => CounitalCoalgebra r QuaternionBasis where
  counit f = f E + f I + f J + f K

{-
-- dual quaternion comultiplication
instance (TriviallyInvolutive r, Rng r) => Coalgebra r QuaternionBasis where
  comult f = f' where
    fe = f E
    fi = f I
    fj = f J
    fk = f K
    fe' = negate fe
    fi' = negate fi
    fj' = negate fj
    fk' = negate fk
    f' E E = fe
    f' E I = fi
    f' E J = fj
    f' E K = fk
    f' I E = fi
    f' I I = fe'
    f' I J = fk
    f' I K = fj'
    f' J E = fj
    f' J I = fk'
    f' J J = fe'
    f' J K = fi
    f' K E = fk
    f' K I = fj
    f' K J = fi'
    f' K K = fe'

instance (TriviallyInvolutive r, Rng r) => CounitalCoalgebra r QuaternionBasis where
  counit f = f E
-}

instance (TriviallyInvolutive r, Rng r)  => Bialgebra r QuaternionBasis 

instance (TriviallyInvolutive r, InvolutiveSemiring r, Rng r)  => InvolutiveAlgebra r QuaternionBasis where
  inv f E = f E
  inv f b = negate (f b)

instance (TriviallyInvolutive r, InvolutiveSemiring r, Rng r) => InvolutiveCoalgebra r QuaternionBasis where
  coinv = inv

instance (TriviallyInvolutive r, InvolutiveSemiring r, Rng r) => HopfAlgebra r QuaternionBasis where
  antipode = inv

instance (TriviallyInvolutive r, Rng r) => Multiplicative (Quaternion r) where
  (*) = mulRep

instance (TriviallyInvolutive r, Rng r) => Semiring (Quaternion r)

instance (TriviallyInvolutive r, Ring r) => Unital (Quaternion r) where
  one = oneRep

instance (TriviallyInvolutive r, Ring r) => Rig (Quaternion r) where
  fromNatural n = Quaternion (fromNatural n) zero zero zero

instance (TriviallyInvolutive r, Ring r) => Ring (Quaternion r) where
  fromInteger n = Quaternion (fromInteger n) zero zero zero

instance ( TriviallyInvolutive r, Rng r) => LeftModule (Quaternion r) (Quaternion r) where 
  (.*) = (*)
instance (TriviallyInvolutive r, Rng r) => RightModule (Quaternion r) (Quaternion r) where 
  (*.) = (*)

instance (TriviallyInvolutive r, Rng r) => InvolutiveMultiplication (Quaternion r) where
  -- without trivial involution, multiplication fails associativity, and we'd need to 
  -- support weaker multiplicative properties like Alternative and PowerAssociative
  adjoint (Quaternion a b c d) = Quaternion a (negate b) (negate c) (negate d)

-- | Cayley-Dickson quaternion isomorphism (one way)
complicate :: Complicated c => QuaternionBasis -> (c,c)
complicate E = (e, e)
complicate I = (i, e) 
complicate J = (e, i)
complicate K = (i, i)

scalarPart :: (Representable f, Key f ~ QuaternionBasis) => f r -> r
scalarPart f = index f E

vectorPart :: (Representable f, Key f ~ QuaternionBasis) => f r -> (r,r,r)
vectorPart f = (index f I, index f J, index f K)

instance (TriviallyInvolutive r, Rng r) => Quadrance r (Quaternion r) where
  quadrance n = scalarPart (adjoint n * n)

instance (TriviallyInvolutive r, Ring r, Division r) => Division (Quaternion r) where
  recip q@(Quaternion a b c d) = Quaternion (qq \\ a) (qq \\ b) (qq \\ c) (qq \\ d)
    where qq = quadrance q
