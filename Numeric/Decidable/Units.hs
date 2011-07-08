module Numeric.Decidable.Units 
  ( DecidableUnits(..)
  , powDecidableUnits
  , recipUnitIntegral
  ) where

import Data.Maybe (isJust)
import Data.Int
import Data.Word
import Numeric.Multiplicative.Class
import Numeric.Multiplicative.Monoid.Class
import Prelude hiding ((*))

class MultiplicativeMonoid r => DecidableUnits r where
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
recipUnitIntegral 1 = Just 1
recipUnitIntegral (-1) = Just (-1)
recipUnitIntegral _ = Nothing

powDecidableUnits :: (DecidableUnits r, Integral n) => r -> n -> r
powDecidableUnits x0 y0 = case compare y0 0 of
  LT -> case recipUnit x0 of
    Nothing -> error "non-unit recipriocal" 
    Just x0' -> f x0' (negate y0)
  EQ -> one
  GT -> f x0 y0
  where
    f x y 
      | even y = f (x * x) (y `quot` 2)
      | y == 1 = x
      | otherwise = g (x * x) ((y - 1) `quot` 2) x
    g x y z 
      | even y = g (x * x) (y `quot` 2) z
      | y == 1 = x * z
      | otherwise = g (x * x) ((y - 1) `quot` 2) (x * z)

instance DecidableUnits Integer where recipUnit = recipUnitIntegral
instance DecidableUnits Int where recipUnit = recipUnitIntegral
instance DecidableUnits Int8 where recipUnit = recipUnitIntegral
instance DecidableUnits Int16 where recipUnit = recipUnitIntegral
instance DecidableUnits Int32 where recipUnit = recipUnitIntegral
instance DecidableUnits Int64 where recipUnit = recipUnitIntegral
instance DecidableUnits Word where recipUnit = recipUnitIntegral
instance DecidableUnits Word8 where recipUnit = recipUnitIntegral
instance DecidableUnits Word16 where recipUnit = recipUnitIntegral
instance DecidableUnits Word32 where recipUnit = recipUnitIntegral
instance DecidableUnits Word64 where recipUnit = recipUnitIntegral
