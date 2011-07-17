module Numeric.Band.Class
  ( 
  -- * Multiplicative Bands
    Band
  , pow1pBand
  , powBand
  ) where

import Numeric.Multiplicative
import Numeric.Multiplication.Unital
import Numeric.Natural

-- | An multiplicative semigroup with idempotent multiplication.
--
-- > a * a = a
class Multiplicative r => Band r

pow1pBand :: Whole n => r -> n -> r
pow1pBand r _ = r 

powBand :: (Unital r, Whole n) => r -> n -> r
powBand _ 0 = one
powBand r _ = r

instance Band ()
instance Band Bool
instance (Band a, Band b) => Band (a,b)
instance (Band a, Band b, Band c) => Band (a,b,c)
instance (Band a, Band b, Band c, Band d) => Band (a,b,c,d)
instance (Band a, Band b, Band c, Band d, Band e) => Band (a,b,c,d,e)
