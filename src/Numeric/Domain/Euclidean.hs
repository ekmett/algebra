{-# LANGUAGE CPP, ConstraintKinds, FlexibleContexts, FlexibleInstances     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes #-}
{-# LANGUAGE RebindableSyntax, UndecidableInstances                        #-}
module Numeric.Domain.Euclidean (Euclidean(..), prs, normalize, gcd', leadingUnit, chineseRemainder) where
import Numeric.Additive.Group
import Numeric.Algebra.Class
import Numeric.Algebra.Unital
import Numeric.Decidable.Units
import Numeric.Decidable.Zero
import Numeric.Domain.Class
import Numeric.Natural (Natural)
import Numeric.Ring.Class
import Prelude (Eq (..), Integer, Maybe (..), abs)
import Prelude (fst, otherwise, fail)
import Prelude (signum, snd, ($), (.))
import qualified Prelude as P

infixl 7 `quot`, `rem`
infix  7 `divide`
class (Ring r, DecidableZero r, DecidableUnits r, Domain r) => Euclidean r where
  -- | @splitUnit r@ calculates its leading unit and normal form.
  --
  -- prop> let (u, n) = splitUnit r in r == u * n && fst (splitUnit n) == one && isUnit u
  splitUnit :: r -> (r, r)
  -- | Euclidean (degree) function on @r@.
  degree :: r -> Maybe Natural
  -- | Division algorithm. @a `divide` b@ calculates
  --   quotient and reminder of @a@ divided by @b@.
  --
  -- prop> let (q, r) = divide a p in p*q + r == a && degree r < degree q
  divide :: r                   -- ^ elements divided by
         -> r                   -- ^ divisor
         -> (r,r)               -- ^ quotient and remin
  quot :: r -> r -> r
  quot a b = fst $ a `divide` b
  {-# INLINE quot #-}

  rem :: r -> r -> r
  rem a b = snd $ a `divide` b
  {-# INLINE rem #-}

  -- | @'gcd' a b@ calculates greatest common divisor of @a@ and @b@.
  gcd :: r -> r -> r
  gcd a b = let (g,_,_):_ = euclid a b
            in g
  {-# INLINE gcd #-}

  -- | Extended euclidean algorithm.
  --
  -- prop> euclid f g == xs ==> all (\(r, s, t) -> r == f * s + g * t) xs
  euclid :: r -> r -> [(r,r,r)]
  euclid f g =
    let (ug, g') = splitUnit g
        Just t'  = recipUnit ug
        (uf, f') = splitUnit f
        Just s   = recipUnit uf
    in step [(g', 0, t'), (f', s, 0)]
    where
      step acc@((r',s',t'):(r,s,t):_)
        | isZero r' = P.tail acc
        | otherwise =
          let q         = r `quot` r'
              (ur, r'') = splitUnit $ r - q * r'
              Just u    = recipUnit ur
              s''       = (s - q * s') * u
              t''       = (t - q * t') * u
          in step ((r'', s'', t'') : acc)
      step _ = P.error "cannot happen!"
#if (__GLASGOW_HASKELL__ > 708)
  {-# MINIMAL splitUnit, degree, divide #-}
#endif

prs :: Euclidean r => r -> r -> [(r, r, r)]
prs f g = step [(g, 0, 1), (f, 1, 0)]
  where
    step acc@((r',s',t'):(r,s,t):_)
      | isZero r' = P.tail acc
      | otherwise =
        let q         = r `quot` r'
            s''       = (s - q * s')
            t''       = (t - q * t')
        in step ((r - q * r', s'', t'') : acc)
    step _ = P.error "cannot happen!"

gcd' :: Euclidean r => [r] -> r
gcd' []     = one
gcd' [x]    = leadingUnit x
gcd' [x,y]  = gcd x y
gcd' (x:xs) = gcd x (gcd' xs)

normalize :: Euclidean r => r -> r
normalize = snd . splitUnit

leadingUnit :: Euclidean r => r -> r
leadingUnit = fst . splitUnit

instance Euclidean Integer where
  splitUnit 0 = (1, 0)
  splitUnit n = (signum n, abs n)
  {-# INLINE splitUnit #-}

  degree = Just . P.fromInteger . abs
  {-# INLINE degree #-}

  divide = P.divMod
  {-# INLINE divide #-}

chineseRemainder :: Euclidean r
                 => [(r, r)] -- ^ List of @(m_i, v_i)@
                 -> r        -- ^ @f@ with @f@ = @v_i@ (mod @v_i@)
chineseRemainder mvs =
  let (ms, _) = P.unzip mvs
      m = product ms
  in sum [((vi*s) `rem` mi)*n | (mi, vi) <- mvs
                               , let n = m `quot` mi
                               , let (_, s, _) : _ = euclid n mi
                               ]
