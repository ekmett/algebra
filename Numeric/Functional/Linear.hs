{-# LANGUAGE ImplicitParams #-}
module Numeric.Functional.Linear 
  ( Linear(..)
  , (.*), (*.)
  , embedHom
  , augmentHom
  ) where

import Numeric.Addition
import Numeric.Multiplication
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

-- TODO: check if this the monoid ring? 
instance (Semiring r, Multiplicative a) => Multiplicative (Linear r a) where
  Linear m * Linear n = Linear (\k -> m (\a -> n (\b -> k (a * b))))
instance (Commutative r, Ring r, Commutative a) => Commutative (Linear r a)
instance (Semiring r, Multiplicative a) => Semiring (Linear r a)
instance (Semiring r, MultiplicativeMonoid a) => MultiplicativeMonoid (Linear r a) where
  one = return one
instance (Rig r, MultiplicativeMonoid a) => Rig (Linear r a)
instance (Rng r, MultiplicativeMonoid a) => Rng (Linear r a)
instance (Ring r, MultiplicativeMonoid a) => Ring (Linear r a)

-- ring homomorphism from r -> r[a]
embedHom :: (Multiplicative s, MultiplicativeMonoid a) => s -> Linear s a 
embedHom r = Linear (\k -> r * k one)

-- if the characteristic of s does not divide the order of a, then s[a] is semisimple
-- and if a has a length function, we can build a filtered algebra

-- | The augmentation ring homomorphism from r[a] -> r
augmentHom :: MultiplicativeMonoid s => Linear s a -> s
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

infixl 7 .*, *.
-- scalar multiplication is tricky because we don't have MPTCs in this package, so we provide the one-off combinator
(.*) :: Multiplicative s => s -> Linear s a -> Linear s a
s .* Linear m = Linear (\k -> s * m k)

(*.) :: Multiplicative s => Linear s a -> s -> Linear s a
Linear m *. s = Linear (\k -> m k * s)

-- instance MultiplicativeSemigroup s => LeftModule s (Linear s a) where
--  s .* Linear m = Linear (s .* m)

-- instance MultiplicativeSemigroup s => RightModule s (Linear s a) where
--  Linear m *. s = Linear (m *. s)

