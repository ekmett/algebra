{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Numeric.Algebra.Unital
  ( UnitalAlgebra(..)
  , CounitalCoalgebra(..)
  ) where

import Numeric.Algebra.Class
import Numeric.Multiplication.Unital.Internal
import Data.Sequence (Seq)
import Numeric.Semiring.Internal
import qualified Data.Sequence as Seq
import Prelude (($))

-- A coassociative counital coalgebra over a semiring, where the module is free
class Coalgebra r c => CounitalCoalgebra r c where
  counit :: (c -> r) -> r

instance (Unital r, UnitalAlgebra r m) => CounitalCoalgebra r (m -> r) where
  counit k = k one

instance (UnitalAlgebra r a, CounitalCoalgebra r c) => CounitalCoalgebra (a -> r) c where 
  counit k a = counit (`k` a)

instance CounitalCoalgebra () a where
  counit _ = ()

instance (CounitalCoalgebra r a, CounitalCoalgebra r b) => CounitalCoalgebra r (a, b) where
  counit k = counit $ \a -> counit $ \b -> k (a,b)

instance (CounitalCoalgebra r a, CounitalCoalgebra r b, CounitalCoalgebra r c) => CounitalCoalgebra r (a, b, c) where
  counit k = counit $ \a -> counit $ \b -> counit $ \c -> k (a,b,c)

instance (CounitalCoalgebra r a, CounitalCoalgebra r b, CounitalCoalgebra r c, CounitalCoalgebra r d) => CounitalCoalgebra r (a, b, c, d) where
  counit k = counit $ \a -> counit $ \b -> counit $ \c -> counit $ \d -> k (a,b,c,d)

instance (CounitalCoalgebra r a, CounitalCoalgebra r b, CounitalCoalgebra r c, CounitalCoalgebra r d, CounitalCoalgebra r e) => CounitalCoalgebra r (a, b, c, d, e) where
  counit k = counit $ \a -> counit $ \b -> counit $ \c -> counit $ \d -> counit $ \e -> k (a,b,c,d,e)

instance Semiring r => CounitalCoalgebra r [a] where
  counit k = k []

instance Semiring r => CounitalCoalgebra r (Seq a) where
  counit k = k (Seq.empty)
