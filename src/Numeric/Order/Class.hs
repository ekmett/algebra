module Numeric.Order.Class 
  ( Order(..)
  , orderOrd
  ) where

import Data.Int
import Data.Word
import Data.Set
import Numeric.Natural

-- a partial order (a, <=)
class Order a where
  (<~) :: a -> a -> Bool
  a <~ b = maybe False (<= EQ) (order a b)

  (<) :: a -> a -> Bool
  a < b = order a b == Just LT

  (>~) :: a -> a -> Bool
  a >~ b = b <~ a

  (>) :: a -> a -> Bool
  a > b = order a b == Just GT

  (~~) :: a -> a -> Bool
  a ~~ b = order a b == Just EQ

  (/~) :: a -> a -> Bool
  a /~ b = order a b /= Just EQ

  order :: a -> a -> Maybe Ordering
  order a b 
    | a <~ b = Just $ if b <~ a 
               then EQ
               else LT
    | b <~ a = Just GT
    | otherwise = Nothing

  comparable :: a -> a -> Bool
  comparable a b = maybe False (const True) (order a b)


orderOrd :: Ord a => a -> a -> Maybe Ordering
orderOrd a b = Just (compare a b)

instance Order Bool where order = orderOrd 
instance Order Integer where order = orderOrd 
instance Order Int where order = orderOrd 
instance Order Int8 where order = orderOrd 
instance Order Int16 where order = orderOrd 
instance Order Int32 where order = orderOrd 
instance Order Int64 where order = orderOrd 
instance Order Natural where order = orderOrd 
instance Order Word where order = orderOrd
instance Order Word8 where order = orderOrd
instance Order Word16 where order = orderOrd
instance Order Word32 where order = orderOrd
instance Order Word64 where order = orderOrd
instance Ord a => Order (Set a) where
  (<~) = isSubsetOf

instance Order () where 
  order _ _ = Just EQ
  _ <~ _ = True
  comparable _ _ = True

instance (Order a, Order b) => Order (a, b) where 
  (a,b) <~ (i,j) = a <~ i && b <~ j

instance (Order a, Order b, Order c) => Order (a, b, c) where 
  (a,b,c) <~ (i,j,k) = a <~ i && b <~ j && c <~ k

instance (Order a, Order b, Order c, Order d) => Order (a, b, c, d) where 
  (a,b,c,d) <~ (i,j,k,l) = a <~ i && b <~ j && c <~ k && d <~ l

instance (Order a, Order b, Order c, Order d, Order e) => Order (a, b, c, d, e) where 
  (a,b,c,d,e) <~ (i,j,k,l,m) = a <~ i && b <~ j && c <~ k && d <~ l && e <~ m
