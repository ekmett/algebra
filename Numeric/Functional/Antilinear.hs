module Numeric.Functional.Antilinear 
  ( Antilinear(..)
  ) where

import Numeric.Addition
import Control.Applicative
import Control.Monad
import Data.Functor.Plus hiding (zero)
import qualified Data.Functor.Plus as Plus
import Data.Functor.Bind
import qualified Prelude
import Prelude hiding ((+),(-),negate,subtract,replicate)

-- | Antilinear functionals from elements of a free module to a scalar

-- appAntilinear f (x + y) = appAntilinear f x + appAntilinear f y
-- appAntilinear f (a .* x) = adjoint a * appAntilinear f x

newtype Antilinear s a = Antilinear { appAntilinear :: (a -> s) -> s }

instance Functor (Antilinear s) where
  fmap f (Antilinear m) = Antilinear (\k -> m (k . f))

instance Apply (Antilinear s) where
  Antilinear mf <.> Antilinear ma = Antilinear (\k -> mf (\f -> ma (k . f)))

instance Applicative (Antilinear s) where
  pure a = Antilinear (\k -> k a)
  Antilinear mf <*> Antilinear ma = Antilinear (\k -> mf (\f -> ma (k . f)))

instance Bind (Antilinear s) where
  Antilinear m >>- f = Antilinear (\k -> m (\a -> appAntilinear (f a) k))
  
instance Monad (Antilinear s) where
  return a = Antilinear (\k -> k a)
  Antilinear m >>= f = Antilinear (\k -> m (\a -> appAntilinear (f a) k))

instance Additive s => Alt (Antilinear s) where
  Antilinear m <!> Antilinear n = Antilinear (m + n)

instance AdditiveMonoid s => Plus (Antilinear s) where
  zero = Antilinear zero 

instance AdditiveMonoid s => Alternative (Antilinear s) where
  Antilinear m <|> Antilinear n = Antilinear (m + n)
  empty = Antilinear zero

instance AdditiveMonoid s => MonadPlus (Antilinear s) where
  Antilinear m `mplus` Antilinear n = Antilinear (m + n)
  mzero = Antilinear zero

instance Additive s => Additive (Antilinear s a) where
  Antilinear m + Antilinear n = Antilinear (m + n)
  replicate1p n (Antilinear m) = Antilinear (replicate1p n m)

instance AdditiveMonoid s => AdditiveMonoid (Antilinear s a) where
  zero = Antilinear zero
  replicate n (Antilinear m) = Antilinear (replicate n m)

instance AdditiveGroup s => AdditiveGroup (Antilinear s a) where
  Antilinear m - Antilinear n = Antilinear (m - n)
  negate (Antilinear m) = Antilinear (negate m)
  subtract (Antilinear m) (Antilinear n) = Antilinear (subtract m n)
  times n (Antilinear m) = Antilinear (times n m)

instance Abelian s => Abelian (Antilinear s a)

-- instance MultiplicativeSemigroup s => LeftModule s (Antilinear s a) where
--  s .* Antilinear m = Antilinear (s .* m)

-- instance MultiplicativeSemigroup s => RightModule s (Antilinear s a) where
--  Antilinear m *. s = Antilinear (m *. s)

