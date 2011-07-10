{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Numeric.Map.Linear
  ( Linear(..)
  , (.*), (*.)
  , embed
  , augment
  ) where

import Numeric.Addition
import Numeric.Multiplication
import Numeric.Module
import Numeric.Semiring.Class
import Numeric.Rig.Class
import Numeric.Rng.Class
import Numeric.Ring.Class
import Control.Applicative
import Control.Monad
import Control.Category
import Data.Semigroupoid
import Data.Functor.Plus hiding (zero)
import qualified Data.Functor.Plus as Plus
import Data.Functor.Bind
import Prelude hiding ((+),(-),negate,subtract,replicate,(*),(.),id)

-- | linear maps from elements of a free module to another free module over r
--
-- > appLinear f (x + y) = appLinear f x + appLinear f y
-- > appLinear f (r .* x) = r .* appLinear f x
--
newtype Linear r b a = Linear { appLinear :: (a -> r) -> b -> r }

instance Category (Linear r) where
  id = Linear id
  Linear f . Linear g = Linear (g . f)

instance Semigroupoid (Linear r) where
  Linear f `o` Linear g = Linear (g . f)

instance Functor (Linear r b) where
  fmap f (Linear m) = Linear (\k -> m (k . f))

instance Apply (Linear r b) where
  Linear mf <.> Linear ma = Linear (\k b -> mf (\f -> ma (k . f) b) b)

instance Applicative (Linear r b) where
  pure a = Linear (\k _ -> k a)
  Linear mf <*> Linear ma = Linear (\k b -> mf (\f -> ma (k . f) b) b)

instance Bind (Linear r b) where
  Linear m >>- f = Linear (\k b -> m (\a -> appLinear (f a) k b) b)
  
instance Monad (Linear r b) where
  return a = Linear (\k _ -> k a)
  Linear m >>= f = Linear (\k b -> m (\a -> appLinear (f a) k b) b)

instance Additive r => Alt (Linear r b) where
  Linear m <!> Linear n = Linear (m + n)

instance AdditiveMonoid r => Plus (Linear r b) where
  zero = Linear zero 

instance AdditiveMonoid r => Alternative (Linear r b) where
  Linear m <|> Linear n = Linear (m + n)
  empty = Linear zero

instance AdditiveMonoid r => MonadPlus (Linear r b) where
  Linear m `mplus` Linear n = Linear (m + n)
  mzero = Linear zero

instance Additive r => Additive (Linear r b a) where
  Linear m + Linear n = Linear (m + n)
  replicate1p n (Linear m) = Linear (replicate1p n m)

-- TODO: check if this the monoid ring? 
instance (Semiring r, Multiplicative a) => Multiplicative (Linear r b a) where
  Linear m * Linear n = Linear (\k b -> m (\a -> n (\a2 -> k (a * a2)) b) b)
instance (Commutative r, Ring r, Commutative a) => Commutative (Linear r b a)
instance (Semiring r, Multiplicative a) => Semiring (Linear r b a)
instance (Semiring r, MultiplicativeMonoid a) => MultiplicativeMonoid (Linear r b a) where
  one = return one
instance (Rig r, MultiplicativeMonoid a) => Rig (Linear r b a)
instance (Rng r, MultiplicativeMonoid a) => Rng (Linear r b a)
instance (Ring r, MultiplicativeMonoid a) => Ring (Linear r b a)

-- r-module homomorphism from r^b -> r[a]^b, this is a section for augmentHom
embed :: (Multiplicative s, MultiplicativeMonoid a) => (b -> s) -> Linear s b a 
embed f = Linear (\k b -> f b * k one)

-- if the characteristic of s does not divide the order of a, then s[a] is semisimple
-- and if a has a length function, we can build a filtered algebra

-- | The augmentation r-module homomorphism from r[a]^b -> r^b, this is a retraction of embedHom
augment :: MultiplicativeMonoid s => Linear s b a -> b -> s
augment (Linear m) = m (const one) 

instance AdditiveMonoid s => AdditiveMonoid (Linear s b a) where
  zero = Linear zero
  replicate n (Linear m) = Linear (replicate n m)

instance Abelian s => Abelian (Linear s b a)

instance AdditiveGroup s => AdditiveGroup (Linear s b a) where
  Linear m - Linear n = Linear (m - n)
  negate (Linear m) = Linear (negate m)
  subtract (Linear m) (Linear n) = Linear (subtract m n)
  times n (Linear m) = Linear (times n m)

instance (Multiplicative m, Semiring s) => LeftModule (Linear s b m) (Linear s b m) where (.*) = (*)

instance LeftModule r s => LeftModule r (Linear s b m) where
  s .* Linear m = Linear (\k b -> s .* m k b)

instance (Multiplicative m, Semiring s) => RightModule (Linear s b m) (Linear s b m) where (*.) = (*)

instance RightModule r s => RightModule r (Linear s b m) where
  Linear m *. s = Linear (\k b -> m k b *. s)

