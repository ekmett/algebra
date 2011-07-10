module Numeric.Module.Class 
  ( LeftModule(..)
  , RightModule(..)
  ) where

infixl 7 .*, *.

class (Semiring r, Additive m) => LeftModule r m where
  (.*) :: r -> m -> m

-- instance AdditiveMonoid m => LeftModule Natural m where
--   (.*) = replicate

-- instance AdditiveGroup m => LeftModule Integer m where
--   (.*) = times

class (Semiring r, Additive m) => RightModule r m where
  (*.) :: m -> r -> r

-- instance AdditiveMonoid m => RightModule Natural m where
--   (*.) = flip replicate

-- instance AdditiveGroup m => RightModule Integer m where
--   (*.) = flip times
