Collide
=======

Simple collision detection. Not guaranteed to follow the laws of physics _at this time_.

```haskell
collide :: Collide a b => a -> b -> Bool
```

Matrix of doom
--------------

|                 | **Sphere** | **Convex hull** |
| --------------- | ---------- | --------------- |
| **Sphere**      | **✓**      | **✓**           |
| **Convex hull** | **✓**      | **✓**           |
