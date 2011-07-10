{-
module Numeric.Ring.Monoid
  ( 
  ) where

import Data.Map

newtype MonoidRing r m = MonoidRing [(r,m)]

instance Applicative (MonadRing r m) where
  fmap f (MonoidRing as) = 

instance Additive (MonoidRing r m) where
  MonoidRing as + MonoidRing bs = MonoidRing (as ++ bs)
-- TODO: lazy sumWith1

instance AdditiveMonoid (MonoidRing r m) where
  zero = MonoidRing []
-- TODO: lazy sum

instance (AdditiveGroup r, Semiring r) => AdditiveGroup (MonoidRing r m) where
  negate (MonoidRing as) = MonoidRing [ (negate r,m) | (r,m) <- as ]

instance (Multiplicative m, Multiplicative r) => Multiplicative (MonoidRing r m) where
  MonoidRing as * MonoidRing bs = MonoidRing [ (r * s, m * n) | (r,m) <- as, (s,n) <- bs ]
-- TODO: lazy productWith1

instance (Commutative m, Abelian r, Commutative r) => Commutative (MonoidRing r m)
-}
