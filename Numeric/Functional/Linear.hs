{-# LANGUAGE ImplicitParams, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Numeric.Functional.Linear 
  ( Linear(..)
  , (.*), (*.)
  , embedHom
  , augmentHom
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

-- | Linear functionals from elements of a free module to a scalar

-- appLinear f (x + y) = appLinear f x + appLinear f y
-- appLinear f (a .* x) = a * appLinear f x

newtype Linear r a = Linear { appLinear :: (a -> r) -> r }

instance Functor (Linear r) where
  fmap f (Linear m) = Linear (\k -> m (k . f))

instance Apply (Linear r) where
  Linear mf <.> Linear ma = Linear (\k -> mf (\f -> ma (k . f)))

instance Applicative (Linear r) where
  pure a = Linear (\k -> k a)
  Linear mf <*> Linear ma = Linear (\k -> mf (\f -> ma (k . f)))

instance Bind (Linear r) where
  Linear m >>- f = Linear (\k -> m (\a -> appLinear (f a) k))
  
instance Monad (Linear r) where
  return a = Linear (\k -> k a)
  Linear m >>= f = Linear (\k -> m (\a -> appLinear (f a) k))

instance Additive r => Alt (Linear r) where
  Linear m <!> Linear n = Linear (m + n)

instance AdditiveMonoid r => Plus (Linear r) where
  zero = Linear zero 

instance AdditiveMonoid r => Alternative (Linear r) where
  Linear m <|> Linear n = Linear (m + n)
  empty = Linear zero

instance AdditiveMonoid r => MonadPlus (Linear r) where
  Linear m `mplus` Linear n = Linear (m + n)
  mzero = Linear zero

instance Additive r => Additive (Linear r a) where
  Linear m + Linear n = Linear (m + n)
  replicate1p n (Linear m) = Linear (replicate1p n m)

instance FreeCoalgebra r m => Multiplicative (Linear r m) where
  Linear f * Linear g = Linear (\k -> f (g . cojoin k))
instance (Commutative m, FreeCoalgebra r m) => Commutative (Linear r m)
instance FreeCoalgebra r m => Semiring (Linear r m)
instance FreeCounitalCoalgebra r m => Unital (Linear r m) where
  one = Linear counit
instance (Rig r, FreeCounitalCoalgebra r m) => Rig (Linear r m)
instance (Rng r, FreeCounitalCoalgebra r m) => Rng (Linear r m)
instance (Ring r, FreeCounitalCoalgebra r m) => Ring (Linear r m)

-- ring homomorphism from r -> r^a
embedHom :: (Unital m, FreeCounitalCoalgebra r m) => r -> Linear r m
embedHom r = Linear (\k -> r * k one)

-- if the characteristic of s does not divide the order of a, then s[a] is semisimple
-- and if a has a length function, we can build a filtered algebra

-- | The augmentation ring homomorphism from r^a -> r
augmentHom :: Unital s => Linear s a -> s
augmentHom (Linear m) = m (const one)

-- TODO: we can also build up the augmentation ideal

instance AdditiveMonoid s => AdditiveMonoid (Linear s a) where
  zero = Linear zero
  replicate n (Linear m) = Linear (replicate n m)

instance Abelian s => Abelian (Linear s a)

instance AdditiveGroup s => AdditiveGroup (Linear s a) where
  Linear m - Linear n = Linear (m - n)
  negate (Linear m) = Linear (negate m)
  subtract (Linear m) (Linear n) = Linear (subtract m n)
  times n (Linear m) = Linear (times n m)

instance FreeCoalgebra r m => LeftModule (Linear r m) (Linear r m) where
  (.*) = (*)

instance LeftModule r s => LeftModule r (Linear s m) where
  s .* Linear m = Linear (\k -> s .* m k)

instance FreeCoalgebra r m => RightModule (Linear r m) (Linear r m) where
  (*.) = (*)

instance RightModule r s => RightModule r (Linear s m) where
  Linear m *. s = Linear (\k -> m k *. s)

