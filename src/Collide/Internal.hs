module Collide.Internal where

import Data.Foldable
import Data.Monoid
import Linear
import Prelude hiding (minimum, maximum, sum)

projectVS :: (Functor t, Foldable v, Metric v, Floating a) => t (v a) -> v a -> t a
projectVS vs n = fmap (sum . project n) vs

overlapping :: (Foldable t, Ord a) => t a -> t a -> All
overlapping as bs = All $ not (maximum as < minimum bs || maximum bs < minimum as)