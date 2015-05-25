{-# LANGUAGE ScopedTypeVariables #-}

module Collide.InternalSpec (spec) where

import Control.Lens
import Data.Monoid
import Linear
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Collide.Internal
import VectorGen ()

spec :: Spec
spec = do
    describe "projectVS" $ do
        it "extracts the x components when projected along x axis (V2)" $
            property $ \(vs :: [V2 Double]) -> projectVS vs (unit _x) == (vs & mapped %~ view _x)
        it "extracts the x components when projected along x axis (V3)" $
            property $ \(vs :: [V3 Double]) -> projectVS vs (unit _x) == (vs & mapped %~ view _x)
        it "extracts the x components when projected along x axis (V4)" $
            property $ \(vs :: [V4 Double]) -> projectVS vs (unit _x) == (vs & mapped %~ view _x)
        it "extracts the y components when projected along y axis (V3)" $
            property $ \(vs :: [V3 Double]) -> projectVS vs (unit _y) == (vs & mapped %~ view _y)
        it "extracts the z components when projected along z axis (V3)" $
            property $ \(vs :: [V3 Double]) -> projectVS vs (unit _z) == (vs & mapped %~ view _z)
    describe "overlapping" $ do
        it "returns false if every element in the first set is less than everything in the second" $
            overlapping [1, 2, 3] [5, 6, 7] `shouldBe` All False
        it "returns false if every element in the first set is greater than everything in the second" $
            overlapping [5, 6, 7] [1, 2, 3] `shouldBe` All False
        it "returns true if there exist elements in the first set greater than the smallest element in the second" $
            overlapping [1, 2, 3] [2.5, 3, 5] `shouldBe` All True
        it "returns true if there exist elements in the first set smaller than the largest element in the second" $
            overlapping [2.5, 3, 5] [1, 2, 3] `shouldBe` All True