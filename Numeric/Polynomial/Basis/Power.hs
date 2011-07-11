{-# LANGUAGE TypeOperators, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Numeric.Polynomial.Basis.Power 
  ( 
  -- * Power basis
    (:^)(Power, logPower)
  , (^:)
  -- * Variables
  , W(..), X(..), Y(..), Z(..)
  , x
  , at
  , delta
  , coef
  ) where

import Control.Applicative
import Data.Foldable
import Data.Function (on)
import Data.Proxy
import Data.Reflection
import Data.Functor.Representable.Trie
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Traversable
import Numeric.Addition
import Numeric.Algebra.Free
import Numeric.Multiplication
import Numeric.Decidable.Zero
import Numeric.Decidable.Units
import Numeric.Semiring.Class
import Numeric.Rig.Class
import Numeric.Functional.Linear
import Numeric.Natural.Internal
import Prelude hiding ((^),(+),(-),(*),negate, replicate,subtract)

infixr 8 :^,^:

newtype x:^n = Power { logPower :: n } deriving (Eq,Ord)

-- convenient constructor 
-- X ^: 12
(^:) :: x -> n -> x :^ n
_ ^: n = Power n

data W = W deriving Show; instance Reifies W W where reflect _ = W
  
data X = X deriving Show; instance Reifies X X where reflect _ = X

data Y = Y deriving Show; instance Reifies Y Y where reflect _ = Y

data Z = Z deriving Show; instance Reifies Z Z where reflect _ = Z

instance (Show t, Reifies x t, Show n) => Show (x:^n) where
  showsPrec d p = showParen (d > 8) $
   showsPrec 9 (reflect (proxyX p)) . showString "^:" . showsPrec 8 (logPower p) where
      proxyX :: x:^n -> Proxy x
      proxyX _ = Proxy

instance Functor ((:^) x) where
  fmap f (Power n) = Power (f n)

instance Foldable ((:^) x) where
  foldMap f (Power n) = f n

instance Traversable ((:^) x) where
  traverse f (Power n) = Power <$> f n

instance Foldable1 ((:^) x) where
  foldMap1 f (Power n) = f n

instance Traversable1 ((:^) x) where
  traverse1 f (Power n) = Power <$> f n

instance HasTrie n => HasTrie (x :^ n) where
  type BaseTrie (x :^ n) = BaseTrie n
  embedKey = embedKey . logPower
  projectKey = Power . projectKey

instance Additive n => Multiplicative (x :^ n) where
  Power n * Power m = Power (n + m)
  pow1p (Power n) m = Power (replicate1p m n)

instance AdditiveMonoid n => Unital (x :^ n) where
  one = Power zero
  pow (Power n) m = Power (replicate m n)

instance AdditiveGroup n => MultiplicativeGroup (x :^ n) where
  Power n / Power m = Power (n - m)
  recip (Power n) = Power (negate n)
  Power n \\ Power m = Power (subtract n m)
  Power n ^ m = Power (times m n)

instance DecidableZero n => DecidableUnits (x :^ n) where
  recipUnit (Power n) | isZero n  = Just (Power n)
                      | otherwise = Nothing

instance Partitionable n => Factorable (x :^ n) where
  factorWith f = partitionWith (f `on` Power) . logPower 

instance (Semiring r, Additive n) => FreeCoalgebra r (x :^ n) where
  cojoin f i j = f $ i * j

instance (Semiring r, AdditiveMonoid n) => FreeCounitalCoalgebra r (x :^ n) where
  counit f = f one

instance (Semiring r, Partitionable n) => FreeAlgebra r (x :^ n) where
  join f = sum1 . partitionWith (f `on` Power) . logPower

instance (Semiring r, AdditiveMonoid r, Unital r, DecidableZero n, Partitionable n) => FreeUnitalAlgebra r (x :^ n) where
  unit r (Power n) | isZero n  = r
                   | otherwise = zero

x :: Unital n => Linear r (x:^n)
x = Linear $ \k -> k $ Power one

-- the price of this approach is the loss of Horner's scheme
at :: (Unital r, Whole n) => Linear r (x:^n) -> r -> r
m `at` r = m $* pow r . logPower

delta :: (Rig r, Eq a) => a -> a -> r
delta i j | i == j = one
          | otherwise = zero

-- extract the nth coefficient of a polynomial
coef :: (Rig r, Eq n) => n -> Linear r (x:^n) -> r
coef n m = m $* delta (Power n)

