module Numeric.Partial.Group
  ( PartialGroup(..)
  ) where

import Control.Applicative
import Data.Int
import Data.Word
import Numeric.Partial.Semigroup
import Numeric.Partial.Monoid
import Numeric.Natural

class PartialMonoid a => PartialGroup a where
  pnegate :: a -> Maybe a
  pnegate = pminus pzero

  pminus :: a -> a -> Maybe a
  pminus a b = padd a =<< pnegate b 

  psubtract :: a -> a -> Maybe a
  psubtract a b = pnegate a >>= (`padd` b)

instance PartialGroup Int where
  pnegate = Just . negate

instance PartialGroup Integer where
  pnegate = Just . negate

instance PartialGroup Int8 where
  pnegate = Just . negate

instance PartialGroup Int16 where
  pnegate = Just . negate

instance PartialGroup Int32 where
  pnegate = Just . negate

instance PartialGroup Int64 where
  pnegate = Just . negate

instance PartialGroup Word where
  pnegate = Just . negate

instance PartialGroup Word8 where
  pnegate = Just . negate

instance PartialGroup Word16 where
  pnegate = Just . negate

instance PartialGroup Word32 where
  pnegate = Just . negate

instance PartialGroup Word64 where
  pnegate = Just . negate

instance PartialGroup Natural where
  pnegate 0 = Just 0
  pnegate _ = Nothing
  pminus a b 
    | a < b = Nothing
    | otherwise = Just (a - b)
  psubtract a b 
    | a > b = Nothing
    | otherwise = Just (b - a)

instance PartialGroup () where
  pnegate _ = Just () 
  pminus _ _ = Just ()
  psubtract _ _ = Just ()

instance (PartialGroup a, PartialGroup b) => PartialGroup (a, b) where
  pnegate (a, b) = (,) <$> pnegate a <*> pnegate b
  pminus (a, b) (i, j) = (,) <$> pminus a i <*> pminus b j
  psubtract (a, b) (i, j) = (,) <$> psubtract a i <*> psubtract b j

instance (PartialGroup a, PartialGroup b, PartialGroup c) => PartialGroup (a, b, c) where
  pnegate (a, b, c) = (,,) <$> pnegate a <*> pnegate b <*> pnegate c
  pminus (a, b, c) (i, j, k) = (,,) <$> pminus a i <*> pminus b j <*> pminus c k
  psubtract (a, b, c) (i, j, k) = (,,) <$> psubtract a i <*> psubtract b j <*> psubtract c k

instance (PartialGroup a, PartialGroup b, PartialGroup c, PartialGroup d) => PartialGroup (a, b, c, d) where
  pnegate (a, b, c, d) = (,,,) <$> pnegate a <*> pnegate b <*> pnegate c <*> pnegate d
  pminus (a, b, c, d) (i, j, k, l) = (,,,) <$> pminus a i <*> pminus b j <*> pminus c k <*> pminus d l
  psubtract (a, b, c, d) (i, j, k, l) = (,,,) <$> psubtract a i <*> psubtract b j <*> psubtract c k <*> psubtract d l

instance (PartialGroup a, PartialGroup b, PartialGroup c, PartialGroup d, PartialGroup e) => PartialGroup (a, b, c, d, e) where
  pnegate (a, b, c, d, e) = (,,,,) <$> pnegate a <*> pnegate b <*> pnegate c <*> pnegate d <*> pnegate e
  pminus (a, b, c, d, e) (i, j, k, l, m) = (,,,,) <$> pminus a i <*> pminus b j <*> pminus c k <*> pminus d l <*> pminus e m
  psubtract (a, b, c, d, e) (i, j, k, l, m) = (,,,,) <$> psubtract a i <*> psubtract b j <*> psubtract c k <*> psubtract d l <*> psubtract e m
