module Numeric.Natural.Internal
  ( Natural(..)
  , Whole(..)
  ) where

{-# OPTIONS_HADDOCK hide #-}

import Data.Word
import Text.Read

newtype Natural = Natural { runNatural :: Integer } deriving (Eq,Ord)

instance Show Natural where
  showsPrec d (Natural n) = showsPrec d n

instance Read Natural where
  readPrec = fmap Natural $ step readPrec

instance Num Natural where
  Natural n + Natural m = Natural (n + m)
  Natural n * Natural m = Natural (n * m)
  Natural n - Natural m | result < 0 = error "Natural.(-): negative result"
                        | otherwise  = Natural result
	where result = n - m
  abs (Natural n) = Natural n
  signum (Natural n) = Natural (signum n)
  fromInteger n 
    | n >= 0 = Natural n
    | otherwise = error "Natural.fromInteger: negative"

instance Real Natural where
  toRational (Natural a) = toRational a

instance Enum Natural where
  pred (Natural 0) = error "Natural.pred: 0"
  pred (Natural n) = Natural (pred n)
  succ (Natural n) = Natural (succ n)
  fromEnum (Natural n) = fromEnum n
  toEnum n | n < 0     = error "Natural.toEnum: negative"
           | otherwise = Natural (toEnum n)

instance Integral Natural where
  quot (Natural a) (Natural b) = Natural (quot a b)
  rem (Natural a) (Natural b) = Natural (rem a b)
  div (Natural a) (Natural b) = Natural (div a b)
  mod (Natural a) (Natural b) = Natural (mod a b)
  divMod (Natural a) (Natural b) = (Natural q, Natural r) where (q,r) = divMod a b
  quotRem (Natural a) (Natural b) = (Natural q, Natural r) where (q,r) = quotRem a b
  toInteger = runNatural

class Integral n => Whole n where
  toNatural :: n -> Natural
  unsafePred :: n -> n

instance Whole Word where
  toNatural = Natural . toInteger
  unsafePred n = n - 1

instance Whole Word8 where
  toNatural = Natural . toInteger
  unsafePred n = n - 1

instance Whole Word16 where
  toNatural = Natural . toInteger
  unsafePred n = n - 1

instance Whole Word32 where
  toNatural = Natural . toInteger
  unsafePred n = n - 1

instance Whole Word64 where
  toNatural = Natural . toInteger
  unsafePred n = n - 1

instance Whole Natural where
  toNatural = id
  unsafePred (Natural n) = Natural (n - 1)
