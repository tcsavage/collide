{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE FunctionalDependencies #-}

{-|
Collision detection typeclass and geometric types instances.
-}
module Collide (
-- * Collide typeclasses
  Collide(..)
, defaultCollide
, CollideInfo(..)
, defaultCollideInfo
-- * Sphere
, Sphere(..)
, center
, radius
-- * Convex hull
, ConvexHull(..)
, vertices
, normals
-- * Axis-aligned bounding box
, AABB(..)
, minPoint
, maxPoint
) where

import Control.Lens
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Linear

import Collide.Internal

-- | Defines a binary relation on collidable objects. Expected to be symmetric
-- but not /necessarily/ reflexive.
class Collide a b where
    -- | Are the two objects colliding?
    collide :: a -> b -> Bool

-- | Some types may be able to provide additional information regarding the
-- collision.
class Collide a b => CollideInfo a b c | a b -> c where
    -- | Detects collision, maybe returning collision data.
    collideInfo :: a -> b -> Maybe c

defaultCollide :: CollideInfo a b c => a -> b -> Bool
defaultCollide a = isJust . collideInfo a

defaultCollideInfo :: Collide a b => a -> b -> Maybe ()
defaultCollideInfo a = guard . collide a

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

instance (Metric v, Floating a, Ord a) => CollideInfo (Sphere v a) (Sphere v a) a where
    collideInfo (Sphere p1 r1) (Sphere p2 r2)
        = let x = distance p1 p2 - (r1 + r2)
          in guard (x <= 0) >> Just x

instance (Metric v, Floating a, Ord a) => Collide (Sphere v a) (Sphere v a) where
    collide = defaultCollide

-- | A convex hull consists of two structures. One of vertices, one of normals.
--
-- >>> collider :: ConvexHull [] V3 Double
-- >>> collider = ConvexHull [V3 0 0 0, V3 0 1 0, V3 1 0 0] [V3 0 0 1]
data ConvexHull t v a = ConvexHull { _vertices :: t (v a)
                                   , _normals :: t (v a)
                                   } deriving (Functor, Show, Eq)

-- | Access the underlying structure of vertices.
--
-- >>> let collider = ConvexHull [V2 1 2]
-- >>> collider & vertices . mapped %~ (+ V2 3 1)
-- ConvexHull {_vertices = [V2 4 3]}
vertices :: Lens (ConvexHull t v a) (ConvexHull t v a) (t (v a)) (t (v a))
vertices = lens _vertices (\ch vs -> ch { _vertices = vs })

-- | Access the underlying structure of normals.
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

data AABB v a = AABB { _minPoint :: v a
                     , _maxPoint :: v a
                     } deriving (Functor, Show, Eq)

minPoint :: Lens (AABB v a) (AABB v a) (v a) (v a)
minPoint = lens _minPoint (\a p -> a { _minPoint = p })

maxPoint :: Lens (AABB v a) (AABB v a) (v a) (v a)
maxPoint = lens _maxPoint (\a p -> a { _maxPoint = p })

instance (Traversable v, Metric v, Floating a, Ord a) => Collide (AABB v a) (AABB v a) where
    collide a b = getAll $ foldMap go $ basisFor $ view minPoint a
        where
            go n = projectVS [view minPoint a, view maxPoint a] n `overlapping` projectVS [view minPoint b, view maxPoint b] n