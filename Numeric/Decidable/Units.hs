module Numeric.Decidable.Units 
  ( DecidableUnits(..)
  , recipUnitIntegral
  , recipUnitWhole
  ) where

import Data.Maybe (isJust)
import Data.Int
import Data.Word
import Numeric.Semigroup.Multiplicative
import Numeric.Monoid.Multiplicative
import Numeric.Natural.Internal
import Control.Applicative
import Prelude hiding ((*))

class Unital r => DecidableUnits r where
  recipUnit :: r -> Maybe r

  isUnit :: DecidableUnits r => r -> Bool
  isUnit = isJust . recipUnit

  (^?) :: Integral n => r -> n -> Maybe r
  x0 ^? y0 = case compare y0 0 of
    LT -> fmap (`f` negate y0) (recipUnit x0)
    EQ -> Just one
    GT -> Just (f x0 y0)
    where
        f x y 
            | even y = f (x * x) (y `quot` 2)
            | y == 1 = x
            | otherwise = g (x * x) ((y - 1) `quot` 2) x
        g x y z 
            | even y = g (x * x) (y `quot` 2) z
            | y == 1 = x * z
            | otherwise = g (x * x) ((y - 1) `quot` 2) (x * z)

recipUnitIntegral :: Integral r => r -> Maybe r
recipUnitIntegral a@1 = Just a
recipUnitIntegral a@(-1) = Just a
recipUnitIntegral _ = Nothing

recipUnitWhole :: Integral r => r -> Maybe r
recipUnitWhole a@1 = Just a
recipUnitWhole _ = Nothing

instance DecidableUnits Bool where 
  recipUnit False = Nothing
  recipUnit True = Just True
instance DecidableUnits Integer where recipUnit = recipUnitIntegral
instance DecidableUnits Int where recipUnit = recipUnitIntegral
instance DecidableUnits Int8 where recipUnit = recipUnitIntegral
instance DecidableUnits Int16 where recipUnit = recipUnitIntegral
instance DecidableUnits Int32 where recipUnit = recipUnitIntegral
instance DecidableUnits Int64 where recipUnit = recipUnitIntegral
instance DecidableUnits Natural where recipUnit = recipUnitWhole
instance DecidableUnits Word where recipUnit = recipUnitWhole
instance DecidableUnits Word8 where recipUnit = recipUnitWhole
instance DecidableUnits Word16 where recipUnit = recipUnitWhole
instance DecidableUnits Word32 where recipUnit = recipUnitWhole
instance DecidableUnits Word64 where recipUnit = recipUnitWhole
instance DecidableUnits () where recipUnit _ = Just ()

instance (DecidableUnits a, DecidableUnits b) => DecidableUnits (a, b) where
  recipUnit (a,b) = (,) <$> recipUnit a <*> recipUnit b

instance (DecidableUnits a, DecidableUnits b, DecidableUnits c) => DecidableUnits (a, b, c) where
  recipUnit (a,b,c) = (,,) <$> recipUnit a <*> recipUnit b <*> recipUnit c

instance (DecidableUnits a, DecidableUnits b, DecidableUnits c, DecidableUnits d) => DecidableUnits (a, b, c, d) where
  recipUnit (a,b,c,d) = (,,,) <$> recipUnit a <*> recipUnit b <*> recipUnit c <*> recipUnit d

instance (DecidableUnits a, DecidableUnits b, DecidableUnits c, DecidableUnits d, DecidableUnits e) => DecidableUnits (a, b, c, d, e) where
  recipUnit (a,b,c,d,e) = (,,,,) <$> recipUnit a <*> recipUnit b <*> recipUnit c <*> recipUnit d <*> recipUnit e
