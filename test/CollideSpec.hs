{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

instance (Arbitrary (v a), Arbitrary a) => Arbitrary (ConvexHull [] v a) where
    arbitrary = ConvexHull <$> fmap getNonEmpty arbitrary <*> fmap getNonEmpty arbitrary

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
    describe "sphere-convex hull collsion" $ do
        it "symmetric" $
            property $ \(s :: Sphere V3 Double) (ch :: ConvexHull [] V3 Double) ->
                collide s ch == collide ch s
        it "non-collsion" $
            collide s1 ch1 `shouldBe` False
        it "collsion" $
            collide s2 ch1 `shouldBe` True
    describe "convex hull-convex hull collision" $ do
        it "symmetric" $
            property $ \(ch1 :: ConvexHull [] V3 Double) (ch2 :: ConvexHull [] V3 Double) ->
                collide ch1 ch2 == collide ch2 ch1
        it "test non-collsion" $
            collide ch1 ch2 `shouldBe` False
        it "test collsion" $
            collide ch1 ch3 `shouldBe` True

s1 :: Sphere V3 Double
s1 = Sphere (V3 3 3 3) 1

s2 :: Sphere V3 Double
s2 = Sphere (V3 1 1 1) 1

ch1 :: ConvexHull [] V3 Double
ch1 = ConvexHull { _vertices = [ V3 (-1.332223) 0.331874 (-1.708829)
                               , V3 (-0.149953) (-1.219420) (-2.151234)
                               , V3 (-0.276591) (-1.855482) (-0.259307)
                               , V3 (-1.458862) (-0.304188) 0.183098
                               , V3 0.275943 1.422246 (-1.234603)
                               , V3 1.458213 (-0.129048) (-1.677009)
                               , V3 1.331575 (-0.765110) 0.214918
                               , V3 0.149304 0.786184 0.657324
                               ]
                 , _normals = [ V3 0.063300 0.318000 (-0.946000)
                              , V3 0.591100 (-0.775600) (-0.221200)
                              , V3 (-0.063300) (-0.318000) 0.946000
                              , V3 (-0.591100) 0.775600 0.221200
                              , V3 (-0.804100) (-0.545200) (-0.237100)
                              , V3 0.804100 0.545200 0.237100
                              ]
                 }

ch2 :: ConvexHull [] V3 Double
ch2 = ConvexHull { _vertices = [ V3 (-0.326662) 0.892572 3.641755
                               , V3 (-1.888632) 1.677323 2.669945
                               , V3 (-1.456713) 0.556613 1.070741
                               , V3 0.105257 (-0.228138) 2.042551
                               , V3 0.845385 2.351398 2.935972
                               , V3 (-0.716585) 3.136150 1.964162
                               , V3 (-0.284665) 2.015440 0.364958
                               , V3 1.277305 1.230688 1.336768
                               ]
                 , _normals = [ V3 (-0.216000) 0.560400 0.799600
                              , V3 (-0.781000) 0.392400 (-0.485900)
                              , V3 0.216000 (-0.560400) (-0.799600)
                              , V3 0.781000 (-0.392400) 0.485900
                              , V3 (-0.586000) (-0.729400) 0.352900
                              , V3 0.586000 0.729400 (-0.352900)
                              ]
                 }

ch3 :: ConvexHull [] V3 Double
ch3 = ConvexHull { _vertices = [ V3 (-0.124179) 0.445695 2.3666065
                               , V3 (-1.686149) 1.230446 1.3947955
                               , V3 (-1.254230) 0.109736 (-0.204401)
                               , V3 0.307740 (-0.675015) 0.7674021
                               , V3 1.047868 1.904521 1.660823
                               , V3 (-0.514101) 2.689272 0.6890122
                               , V3 (-0.082182) 1.568562 (-0.910198)
                               , V3 1.479788 0.783811 0.061618
                               ]
                 , _normals = [ V3 (-0.216000) 0.560400 0.799600
                              , V3 (-0.781000) 0.392400 (-0.485900)
                              , V3 0.216000 (-0.560400) (-0.799600)
                              , V3 0.781000 (-0.392400) 0.485900
                              , V3 (-0.586000) (-0.729400) 0.352900
                              , V3 0.586000 0.729400 (-0.352900)
                              ]
                 }
