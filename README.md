Collide
=======

[![Build Status](https://travis-ci.org/tcsavage/collide.svg?branch=master)](https://travis-ci.org/tcsavage/collide)

Simple collision detection. Not guaranteed to follow the laws of physics _at this time_.

```haskell
collide :: Collide a b => a -> b -> Bool
```

Matrix of doom
--------------

| _Collide_       | **Ray** | **Sphere** | **AABB** | **Convex hull** |
| --------------- | ------- | ---------- | -------- | --------------- |
| **Ray**         |         | **✓**      | **✓**    |                 |
| **Sphere**      | **✓**   | **✓**      |          | **✓**           |
| **AABB**        | **✓**   |            | **✓**    |                 |
| **Convex hull** |         | **✓**      |          | **✓**           |

| _CollideInfo_   | **Ray**               | **Sphere**            |
| --------------- | --------------------- | --------------------- |
| **Ray**         |                       | (t, position, normal) |
| **Sphere**      | (t, position, normal) | Overlap distance      |