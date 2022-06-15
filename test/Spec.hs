{-# LANGUAGE TemplateHaskell #-}

import Lib
import Test.QuickCheck

prop_complexId :: (Eq a, Ord a) => SimplicialComplex a -> Bool
prop_complexId sc = fromSimplices (simplices sc) == sc

prop_simplexId :: Ord a => [Simplex a] -> Bool
prop_simplexId s = simplices (fromSimplices s) >= s

instance Arbitrary a => Arbitrary (Simplex a) where
  arbitrary = Simplex <$> arbitrary

instance (Ord a, Arbitrary a) => Arbitrary (SimplicialComplex a) where
  arbitrary = fromSimplices <$> arbitrary

return []
runTests = $verboseCheckAll

main :: IO Bool
main = do
  runTests
