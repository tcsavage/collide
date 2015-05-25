module VectorGen where

import Control.Applicative
import Linear
import Test.QuickCheck

instance Arbitrary a => Arbitrary (V2 a) where
    arbitrary = V2 <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (V3 a) where
    arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (V4 a) where
    arbitrary = V4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary