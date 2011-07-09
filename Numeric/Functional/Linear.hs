{-# LANGUAGE ImplicitParams #-}
module Numeric.Functional.Linear 
  ( Linear(..)
  , (.*)
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

newtype Linear s a = Linear { appLinear :: (a -> s) -> s }

instance Functor (Linear s) where
  fmap f (Linear m) = Linear (\k -> m (k . f))

instance Apply (Linear s) where
  Linear mf <.> Linear ma = Linear (\k -> mf (\f -> ma (k . f)))

instance Applicative (Linear s) where
  pure a = Linear (\k -> k a)
  Linear mf <*> Linear ma = Linear (\k -> mf (\f -> ma (k . f)))

instance Bind (Linear s) where
  Linear m >>- f = Linear (\k -> m (\a -> appLinear (f a) k))
  
instance Monad (Linear s) where
  return a = Linear (\k -> k a)
  Linear m >>= f = Linear (\k -> m (\a -> appLinear (f a) k))

instance Additive s => Alt (Linear s) where
  Linear m <!> Linear n = Linear (m + n)

instance AdditiveMonoid s => Plus (Linear s) where
  zero = Linear zero 

instance AdditiveMonoid s => Alternative (Linear s) where
  Linear m <|> Linear n = Linear (m + n)
  empty = Linear zero

instance AdditiveMonoid s => MonadPlus (Linear s) where
  Linear m `mplus` Linear n = Linear (m + n)
  mzero = Linear zero

instance Additive s => Additive (Linear s a) where
  Linear m + Linear n = Linear (m + n)
  replicate1p n (Linear m) = Linear (replicate1p n m)

-- TODO: check if this the monoid ring? 
instance (Semiring s, Multiplicative a) => Multiplicative (Linear s a) where
  Linear m * Linear n = Linear (\k -> m (\a -> n (\b -> k (a * b))))
instance (Commutative s, Ring s, Commutative a) => Commutative (Linear s a)
instance (Semiring s, Multiplicative a) => Semiring (Linear s a)
instance (Rig r, MultiplicativeMonoid a, Eq a) => MultiplicativeMonoid (Linear r a) where
  one = return one
instance (Rig s, MultiplicativeMonoid a, Eq a) => Rig (Linear s a)
instance (Rng s, MultiplicativeMonoid a, Eq a) => Rng (Linear s a)
instance (Ring s, MultiplicativeMonoid a, Eq a) => Ring (Linear s a)

infixl 7 .*
-- scalar multiplication is tricky because we don't have MPTCs in this package, so we provide a one-off combinator
(.*) :: Multiplicative s => s -> Linear s a -> Linear s a
s .* Linear m = Linear (\k -> s * m k)

instance AdditiveMonoid s => AdditiveMonoid (Linear s a) where
  zero = Linear zero
  replicate n (Linear m) = Linear (replicate n m)

instance Abelian s => Abelian (Linear s a)

instance AdditiveGroup s => AdditiveGroup (Linear s a) where
  Linear m - Linear n = Linear (m - n)
  negate (Linear m) = Linear (negate m)
  subtract (Linear m) (Linear n) = Linear (subtract m n)
  times n (Linear m) = Linear (times n m)

-- instance MultiplicativeSemigroup s => LeftModule s (Linear s a) where
--  s .* Linear m = Linear (s .* m)

-- instance MultiplicativeSemigroup s => RightModule s (Linear s a) where
--  Linear m *. s = Linear (m *. s)

