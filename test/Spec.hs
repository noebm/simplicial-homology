{-# LANGUAGE TemplateHaskell #-}

import Lib
import Test.QuickCheck

prop_SimplexOrdered :: Ord a => Simplex a -> Bool
prop_SimplexOrdered (Simplex []) = True
prop_SimplexOrdered (Simplex xs) = all (uncurry (<)) $ zip (init xs) (tail xs)

prop_complexId :: (Eq a, Ord a) => SimplicialComplex a -> Bool
prop_complexId sc = fromSimplices (simplices sc) == sc

prop_simplexId :: Ord a => [Simplex a] -> Bool
prop_simplexId s = simplices (fromSimplices s) >= s

prop_insertDimension :: Ord a => Simplex a -> Property
prop_insertDimension s =
  length s < 20 ==> -- limit dimension to 20
  dimension (fromSimplices [s]) == length s - 1

instance (Ord a, Arbitrary a) => Arbitrary (Simplex a) where
  arbitrary = fromList <$> arbitrary

instance (Ord a, Arbitrary a) => Arbitrary (SimplicialComplex a) where
  arbitrary = fromSimplices <$> arbitrary

return []
runTests = $quickCheckAll

main :: IO Bool
main = do
  runTests
