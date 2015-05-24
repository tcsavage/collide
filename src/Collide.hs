{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Collide where

import Data.Foldable
import Data.Monoid
import Linear

class Collide a b where
    collide :: a -> b -> Bool

data Sphere v a = Sphere (v a) a

instance (Metric v, Floating a, Ord a) => Collide (Sphere v a) (Sphere v a) where
    collide (Sphere p1 r1) (Sphere p2 r2) = distance p1 p2 > r1 + r2

newtype ConvexHull t v a = ConvexHull (t (v a))

instance (Foldable t, Metric v, Floating a, Ord a) => Collide (ConvexHull t v a) (Sphere v a) where
    collide (ConvexHull vas) (Sphere p r) =
        getAny $ foldMap (Any . (< r) . distance p) vas

instance (Foldable t, Metric v, Floating a, Ord a) => Collide (Sphere v a) (ConvexHull t v a) where
    collide = flip collide