{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
-- * Convex hull
, ConvexHull(..)
-- * Axis-aligned bounding box
, AABB(..)
-- * Ray
, Ray(..)
) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Linear
import Prelude hiding (minimum, maximum)
import qualified Prelude

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

-- | Helper for defining "Collide" instances for types implementing "CollideInfo".
defaultCollide :: CollideInfo a b c => a -> b -> Bool
defaultCollide a = isJust . collideInfo a

-- | Helper for defining "CollideInfo" instances for types only implementing
-- "Collide". Produces unit.
defaultCollideInfo :: Collide a b => a -> b -> Maybe ()
defaultCollideInfo a = guard . collide a

instance (Collide a b) => Collide a [b] where
    collide a = getAny . foldMap (Any . collide a)

-- | Sphere collider. Defined by a center and a radius.
--
-- >>> collider :: Sphere V3 Double
-- >>> collider = Sphere (V3 0 0 5) 1
data Sphere v a = Sphere { center :: v a
                         , radius :: a
                         } deriving (Functor, Show, Eq)

-- | Produces the overlapping distance
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
data ConvexHull t v a = ConvexHull { vertices :: t (v a)
                                   , normals :: t (v a)
                                   } deriving (Functor, Show, Eq)

instance (Foldable t, Metric v, Floating a, Ord a) => Collide (ConvexHull t v a) (Sphere v a) where
    collide ch (Sphere p r) =
        getAny $ foldMap (Any . (< r) . distance p) $ vertices ch

instance (Foldable t, Metric v, Floating a, Ord a) => Collide (Sphere v a) (ConvexHull t v a) where
    collide = flip collide

instance (Functor t, Foldable t, Foldable v, Metric v, Floating a, Ord a) => Collide (ConvexHull t v a) (ConvexHull t v a) where
    collide (ConvexHull avs ans) (ConvexHull bvs bns) = getAll $ foldMap go ans <> foldMap go bns
        where
            go n = projectVS avs n `overlapping` projectVS bvs n

data AABB v a = AABB { minPoint :: v a
                     , maxPoint :: v a
                     } deriving (Functor, Show, Eq)

instance (Traversable v, Metric v, Floating a, Ord a) => Collide (AABB v a) (AABB v a) where
    collide a b = getAll $ foldMap go $ basisFor $ minPoint a
        where
            go n = projectVS [minPoint a, maxPoint a] n `overlapping` projectVS [minPoint b, maxPoint b] n

data Ray v a = Ray { origin    :: v a
                   , direction :: v a
                   } deriving (Functor, Show, Eq)

instance (Num (v a), Fractional (v a), Applicative v, Foldable v, Ord a, Num a) => CollideInfo (Ray v a) (AABB v a) a where
    collideInfo (Ray o d) (AABB lb rt)
        = let dirFrac = recip d
              a = (lb - o) * dirFrac
              b = (rt - o) * dirFrac
              tmin = maximum (min <$> a <*> b)
              tmax = minimum (max <$> a <*> b)
              in if tmax < 0 || tmax < tmin then Nothing else Just tmin

instance (Num (v a), Fractional (v a), Applicative v, Foldable v, Ord a, Num a) => CollideInfo (AABB v a) (Ray v a) a where
    collideInfo = flip collideInfo

instance (Num (v a), Fractional (v a), Applicative v, Foldable v, Ord a, Num a) => Collide (Ray v a) (AABB v a) where
    collide = defaultCollide

instance (Num (v a), Fractional (v a), Applicative v, Foldable v, Ord a, Num a) => Collide (AABB v a) (Ray v a) where
    collide = flip collide

instance (Num (v a), Metric v, Functor v, Floating a, Num a, Epsilon a, Ord a) => CollideInfo (Ray v a) (Sphere v a) (a, v a, v a) where
    collideInfo (Ray ori dir) (Sphere cen rad)
        = let a = norm dir ^^ (2::Int)
              b = 2 * (dir `dot` (ori - cen))
              c = norm (ori - cen) ^^ (2::Int) - rad ^^ (2::Int)
              times = filter (not . nearZero) (roots a b c)  -- Intersection positions along the ray
              in case times of
                [] -> Nothing
                ts -> let t = Prelude.minimum ts
                          pos = positionAtTime ori dir t
                          normal = signorm (pos - cen)
                          in Just (t, pos, normal)

instance (Num (v a), Metric v, Functor v, Floating a, Num a, Epsilon a, Ord a) => CollideInfo (Sphere v a) (Ray v a) (a, v a, v a) where
    collideInfo = flip collideInfo

instance (Num (v a), Metric v, Functor v, Floating a, Num a, Epsilon a, Ord a) => Collide (Ray v a) (Sphere v a) where
    collide = defaultCollide

instance (Num (v a), Metric v, Functor v, Floating a, Num a, Epsilon a, Ord a) => Collide (Sphere v a) (Ray v a) where
    collide = flip collide
