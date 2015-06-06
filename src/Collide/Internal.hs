module Collide.Internal where

import Data.Foldable
import Data.Monoid
import Linear
import Prelude hiding (minimum, maximum, sum)

-- | Project normal along each of the given vectors.
projectVS :: (Functor t, Foldable v, Metric v, Floating a) => t (v a) -> v a -> t a
projectVS vs n = fmap (sum . project n) vs

-- | Do the given sets of points overlap?
overlapping :: (Foldable t, Ord a) => t a -> t a -> All
overlapping as bs = All $ not (maximum as < minimum bs || maximum bs < minimum as)

positionAtTime :: (Num (v a), Num a, Functor v) => v a -> v a -> a -> v a
positionAtTime origin direction t = origin + (direction ^* t)

roots :: (Ord a, Floating a) => a -> a -> a -> [a]
roots a b c
  | discriminant < 0 = []
  | otherwise = [0.5 * (-b + sqrt discriminant), 0.5 * (-b - sqrt discriminant)]
  where
    discriminant = b^^(2::Int) - 4*a*c