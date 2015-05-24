{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Collide where

import Control.Lens
import Data.Foldable
import Data.Monoid
import Linear

-- | Defines a binary relation on collidable objects. Expected to be symmetric
-- but not /necessarily/ reflexive.
class Collide a b where
    -- | Are the two objects colliding?
    collide :: a -> b -> Bool

-- | Sphere collider. Defined by a center and a radius.
--
-- >>> collider :: Sphere V3 Double
-- >>> collider = Sphere (V3 0 0 5) 1
data Sphere v a = Sphere (v a) a deriving (Functor)

-- | Access the center point of the sphere. Possibly changing its vector type,
-- but not the component type.
center :: Lens (Sphere v1 a) (Sphere v2 a) (v1 a) (v2 a)
center = lens (\(Sphere p _) -> p) (\(Sphere _ r) p' -> Sphere p' r)

-- | Access the radius of the sphere.
radius :: Lens (Sphere v a) (Sphere v a) a a
radius = lens (\(Sphere _ r) -> r) (\(Sphere p _) r' -> Sphere p r')

instance (Metric v, Floating a, Ord a) => Collide (Sphere v a) (Sphere v a) where
    collide (Sphere p1 r1) (Sphere p2 r2) = distance p1 p2 > r1 + r2

-- | A convex hull can be any "Foldable" structure of vertices.
--
-- >>> collider :: ConvexHull [] V3 Double
-- >>> collider = ConvexHull [V3 0 0 0, V3 0 1 0, V3 1 0 0]
newtype ConvexHull t v a = ConvexHull { _vertices :: (t (v a)) } deriving (Functor, Show)

-- | Access the underlying "Foldable" structure, possibly changing its type.
--
-- >>> let collider = ConvexHull [V2 1 2]
-- >>> collider & vertices . mapped %~ (+ V2 3 1)
-- ConvexHull {_vertices = [V2 4 3]}
vertices :: Lens (ConvexHull t1 v1 a1) (ConvexHull t2 v2 a2) (t1 (v1 a1)) (t2 (v2 a2))
vertices = lens _vertices (\ch vs -> ch { _vertices = vs })

instance (Foldable t, Metric v, Floating a, Ord a) => Collide (ConvexHull t v a) (Sphere v a) where
    collide (ConvexHull vas) (Sphere p r) =
        getAny $ foldMap (Any . (< r) . distance p) vas

instance (Foldable t, Metric v, Floating a, Ord a) => Collide (Sphere v a) (ConvexHull t v a) where
    collide = flip collide