{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MonadComprehensions #-}

module Collide (
-- * Collide type class
  Collide(..)
-- * Sphere
, Sphere(..)
, center
, radius
-- * Convex hull
, ConvexHull(..)
, vertices
, normals
) where

import Control.Lens
import Data.Foldable
import Data.Monoid
import Linear
import Prelude hiding (minimum, maximum, sum)

-- | Defines a binary relation on collidable objects. Expected to be symmetric
-- but not /necessarily/ reflexive.
class Collide a b where
    -- | Are the two objects colliding?
    collide :: a -> b -> Bool

-- | Sphere collider. Defined by a center and a radius.
--
-- >>> collider :: Sphere V3 Double
-- >>> collider = Sphere (V3 0 0 5) 1
data Sphere v a = Sphere (v a) a
                deriving (Functor, Show, Eq)

-- | Access the center point of the sphere. Possibly changing its vector type,
-- but not the component type.
center :: Lens (Sphere v1 a) (Sphere v2 a) (v1 a) (v2 a)
center = lens (\(Sphere p _) -> p) (\(Sphere _ r) p' -> Sphere p' r)

-- | Access the radius of the sphere.
radius :: Lens (Sphere v a) (Sphere v a) a a
radius = lens (\(Sphere _ r) -> r) (\(Sphere p _) r' -> Sphere p r')

instance (Metric v, Floating a, Ord a) => Collide (Sphere v a) (Sphere v a) where
    collide (Sphere p1 r1) (Sphere p2 r2) = distance p1 p2 > r1 + r2

-- | A convex hull consists of two structures. One of vertices, one of face
-- normals.
--
-- >>> collider :: ConvexHull [] V3 Double
-- >>> collider = ConvexHull [V3 0 0 0, V3 0 1 0, V3 1 0 0] [V3 0 0 1]
data ConvexHull t v a = ConvexHull { _vertices :: (t (v a))
                                   , _normals :: (t (v a))
                                   } deriving (Functor, Show, Eq)

-- | Access the underlying "Foldable" structure, possibly changing its type.
--
-- >>> let collider = ConvexHull [V2 1 2]
-- >>> collider & vertices . mapped %~ (+ V2 3 1)
-- ConvexHull {_vertices = [V2 4 3]}
vertices :: Lens (ConvexHull t v a) (ConvexHull t v a) (t (v a)) (t (v a))
vertices = lens _vertices (\ch vs -> ch { _vertices = vs })

-- | Access the underlying "Foldable" structure, possibly changing its type.
normals :: Lens (ConvexHull t v a) (ConvexHull t v a) (t (v a)) (t (v a))
normals = lens _normals (\ch ns -> ch { _normals = ns })

instance (Foldable t, Metric v, Floating a, Ord a) => Collide (ConvexHull t v a) (Sphere v a) where
    collide ch (Sphere p r) =
        getAny $ foldMap (Any . (< r) . distance p) $ _vertices ch

instance (Foldable t, Metric v, Floating a, Ord a) => Collide (Sphere v a) (ConvexHull t v a) where
    collide = flip collide

instance (Functor t, Foldable t, Foldable v, Metric v, Floating a, Ord a) => Collide (ConvexHull t v a) (ConvexHull t v a) where
    collide (ConvexHull avs ans) (ConvexHull bvs bns) = getAll $ foldMap go ans <> foldMap go bns
        where
            go n = projectVS avs n `overlapping` projectVS bvs n

projectVS :: (Functor t, Foldable v, Metric v, Floating a) => t (v a) -> v a -> t a
projectVS vs n = fmap (sum . project n) vs

overlapping :: (Foldable t, Ord a) => t a -> t a -> All
overlapping as bs = All (maximum as < minimum bs || maximum bs < minimum bs)