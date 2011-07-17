{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Numeric.Algebra.Involutive
  ( InvolutiveAlgebra(..)
  , InvolutiveCoalgebra(..)
  , InvolutiveBialgebra(..)
  ) where

import Numeric.Algebra.Class
import Numeric.Algebra.Unital
import Numeric.Multiplication.Involutive.Internal

class Coalgebra r c => InvolutiveCoalgebra r c where
  coinv :: (c -> r) -> c -> r
instance InvolutiveCoalgebra () c where
  coinv _ _ = ()
instance (Algebra r b, InvolutiveCoalgebra r h) => InvolutiveCoalgebra (b -> r) c where
  coinv f c a = coinv (`f` a) c
instance (InvolutiveCoalgebra r a, InvolutiveCoalgebra r b) => InvolutiveCoalgebra r (a, b) where
  coinv f (a,b) = coinv (\a' -> coinv (\b' -> f (a',b')) b) a
instance (InvolutiveCoalgebra r a, InvolutiveCoalgebra r b, InvolutiveCoalgebra r c) => InvolutiveCoalgebra r (a, b, c)
  coinv f (a,b,c) = coinv (\a' -> coinv (\b' -> coinv (\c' -> f (a',b',c')) c) b) a
instance (InvolutiveCoalgebra r a, InvolutiveCoalgebra r b, InvolutiveCoalgebra r c, InvolutiveCoalgebra r d) => InvolutiveCoalgebra r (a, b, c, d)
  coinv f (a,b,c,d) = coinv (\a' -> coinv (\b' -> coinv (\c' -> coinv (\d' -> f (a',b',c',d')) d) c) b) a
instance (InvolutiveCoalgebra r a, InvolutiveCoalgebra r b, InvolutiveCoalgebra r c, InvolutiveCoalgebra r d, InvolutiveCoalgebra r e) => InvolutiveCoalgebra r (a, b, c, d, e)
  coinv f (a,b,c,d,e) = coinv (\a' -> coinv (\b' -> coinv (\c' -> coinv (\d' -> coinv (\e' -> f (a',b',c',d',e')) e) d) c) b) a

-- instance InvolutiveCoalgebra r h => Involutive (Covector r h)

-- inv = coinv
class (Bialgebra r h, InvolutiveAlgebra r h, InvolutiveCoalgebra r h) => InvolutiveBialgebra r h
instance (Bialgebra r h, InvolutiveAlgebra r h, InvolutiveCoalgebra r h) => InvolutiveBialgebra r h
