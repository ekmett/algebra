{-# LANGUAGE ImplicitParams, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Numeric.Functional.Linear 
  ( Linear(..)
  -- * Vectors
  , Vector
  , unitVector
  -- * Covectors as linear functionals
  , Covector
  , counitCovector
  , embedCovector
  , augmentCovector
  ) where

import Numeric.Addition
import Numeric.Algebra.Free
import Numeric.Multiplication
import Numeric.Module
import Numeric.Semiring.Class
import Numeric.Rig.Class
import Numeric.Rng.Class
import Numeric.Ring.Class
import Control.Applicative
import Control.Monad
import Data.Functor.Plus hiding (zero)
import qualified Data.Functor.Plus as Plus
import Data.Functor.Bind
import qualified Prelude
import Prelude hiding ((+),(-),negate,subtract,replicate,(*))

infixr 0 $*

-- | Linear functionals from elements of a free module to a scalar

-- f $* (x + y) = (f $* x) + (f $* y)
-- f $* (a .* x) = a * (f $* x)

newtype Linear r a = Linear { ($*) :: (a -> r) -> r }

type Covector a r = Linear r a
type Vector = (->)

instance Functor (Linear r) where
  fmap f m = Linear $ \k -> m $* k . f

instance Apply (Linear r) where
  mf <.> ma = Linear $ \k -> mf $* \f -> ma $* k . f

instance Applicative (Linear r) where
  pure a = Linear $ \k -> k a
  mf <*> ma = Linear $ \k -> mf $* \f -> ma $* k . f

instance Bind (Linear r) where
  m >>- f = Linear $ \k -> m $* \a -> f a $* k
  
instance Monad (Linear r) where
  return a = Linear $ \k -> k a
  m >>= f = Linear $ \k -> m $* \a -> f a $* k

instance Additive r => Alt (Linear r) where
  Linear m <!> Linear n = Linear $ m + n

instance AdditiveMonoid r => Plus (Linear r) where
  zero = Linear zero 

instance AdditiveMonoid r => Alternative (Linear r) where
  Linear m <|> Linear n = Linear $ m + n
  empty = Linear zero

instance AdditiveMonoid r => MonadPlus (Linear r) where
  Linear m `mplus` Linear n = Linear $ m + n
  mzero = Linear zero

instance Additive r => Additive (Linear r a) where
  Linear m + Linear n = Linear $ m + n
  replicate1p n (Linear m) = Linear $ replicate1p n m

instance FreeCoalgebra r m => Multiplicative (Linear r m) where
  f * Linear g = Linear $ \k -> f $* g . cojoin k
instance (Commutative m, FreeCoalgebra r m) => Commutative (Linear r m)
instance FreeCoalgebra r m => Semiring (Linear r m)
instance FreeCounitalCoalgebra r m => Unital (Linear r m) where
  one = Linear counit
instance (Rig r, FreeCounitalCoalgebra r m) => Rig (Linear r m)
instance (Rng r, FreeCounitalCoalgebra r m) => Rng (Linear r m)
instance (Ring r, FreeCounitalCoalgebra r m) => Ring (Linear r m)

unitVector :: (FreeUnitalAlgebra r a, Unital r) => a -> r
unitVector = unit one

counitCovector :: FreeCounitalCoalgebra r c => Linear r c
counitCovector = Linear counit

-- ring homomorphism from r -> r^a, generalizes the embedding of a semiring into its monoid semiring
embedCovector :: (Unital m, FreeCounitalCoalgebra r m) => r -> Linear r m
embedCovector r = Linear $ \k -> r * k one

-- if the characteristic of s does not divide the order of a, then s[a] is semisimple
-- and if a has a length function, we can build a filtered algebra

-- | The augmentation ring homomorphism from r^a -> r, generalizes the augmentation homomorphism from a monoid semiring to the underlying semiring
augmentCovector :: Unital s => Linear s a -> s
augmentCovector m = m $* const one

-- TODO: we can also build up the augmentation ideal

instance AdditiveMonoid s => AdditiveMonoid (Linear s a) where
  zero = Linear zero
  replicate n (Linear m) = Linear (replicate n m)

instance Abelian s => Abelian (Linear s a)

instance AdditiveGroup s => AdditiveGroup (Linear s a) where
  Linear m - Linear n = Linear $ m - n
  negate (Linear m) = Linear $ negate m
  subtract (Linear m) (Linear n) = Linear $ subtract m n
  times n (Linear m) = Linear $ times n m

instance FreeCoalgebra r m => LeftModule (Linear r m) (Linear r m) where
  (.*) = (*)

instance LeftModule r s => LeftModule r (Linear s m) where
  s .* m = Linear $ \k -> s .* (m $* k)

instance FreeCoalgebra r m => RightModule (Linear r m) (Linear r m) where
  (*.) = (*)

instance RightModule r s => RightModule r (Linear s m) where
  m *. s = Linear $ \k -> (m $* k) *. s

