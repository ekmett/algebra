{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Numeric.Algebra.Free.Unital
  ( FreeUnitalAlgebra(..)
  , FreeCounitalCoalgebra(..)
  ) where

import Numeric.Algebra.Free.Class
import Numeric.Monoid.Multiplicative.Internal
import Prelude (($))

-- A coassociative counital coalgebra over a semiring, where the module is free
class FreeCoalgebra r c => FreeCounitalCoalgebra r c where
  counit :: (c -> r) -> r

instance FreeUnitalAlgebra r m => FreeCounitalCoalgebra r (m -> r) where
  counit k = k one

instance (FreeUnitalAlgebra r a, FreeCounitalCoalgebra r c) => FreeCounitalCoalgebra (a -> r) c where 
  counit k a = counit (`k` a)

instance FreeCounitalCoalgebra () a where
  counit _ = ()

instance (FreeCounitalCoalgebra r a, FreeCounitalCoalgebra r b) => FreeCounitalCoalgebra r (a, b) where
  counit k = counit $ \a -> counit $ \b -> k (a,b)

instance (FreeCounitalCoalgebra r a, FreeCounitalCoalgebra r b, FreeCounitalCoalgebra r c) => FreeCounitalCoalgebra r (a, b, c) where
  counit k = counit $ \a -> counit $ \b -> counit $ \c -> k (a,b,c)

instance (FreeCounitalCoalgebra r a, FreeCounitalCoalgebra r b, FreeCounitalCoalgebra r c, FreeCounitalCoalgebra r d) => FreeCounitalCoalgebra r (a, b, c, d) where
  counit k = counit $ \a -> counit $ \b -> counit $ \c -> counit $ \d -> k (a,b,c,d)

instance (FreeCounitalCoalgebra r a, FreeCounitalCoalgebra r b, FreeCounitalCoalgebra r c, FreeCounitalCoalgebra r d, FreeCounitalCoalgebra r e) => FreeCounitalCoalgebra r (a, b, c, d, e) where
  counit k = counit $ \a -> counit $ \b -> counit $ \c -> counit $ \d -> counit $ \e -> k (a,b,c,d,e)
