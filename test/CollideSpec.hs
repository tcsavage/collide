{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CollideSpec (spec) where

import Control.Applicative
import Linear
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Collide
import VectorGen ()

instance (Arbitrary (v a), Arbitrary a) => Arbitrary (Sphere v a) where
    arbitrary = Sphere <$> arbitrary <*> arbitrary

spec :: Spec
spec = do
    describe "sphere-sphere collision" $ do
        it "symmetric" $
            property $ \(s1 :: Sphere V3 Double) (s2 :: Sphere V3 Double) ->
                collide s1 s2 == collide s2 s1
        it "sphere radius 5 at (1 1 1) intersects sphere radius 3 at (3 2 2)" $
            collide (Sphere (V3 (1::Double) 1 1) 5) (Sphere (V3 (3::Double) 2 2) 3) `shouldBe` True
        it "sphere radius 5 at (1 1 1) does not intersect sphere radius 3 at (3 2 10)" $
            collide (Sphere (V3 (1::Double) 1 1) 5) (Sphere (V3 (3::Double) 2 10) 3) `shouldBe` False
