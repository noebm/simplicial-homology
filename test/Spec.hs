{-# LANGUAGE TemplateHaskell #-}

import Lib
import Test.QuickCheck

prop_SimplexOrdered :: Ord a => Simplex a -> Bool
prop_SimplexOrdered s | xs <- toList s =
  case xs of
    [] -> True
    _  -> all (uncurry (<)) $ zip (init xs) (tail xs)

prop_complexId :: (Eq a, Ord a) => SimplicialComplex a -> Bool
prop_complexId sc = fromSimplices (simplices sc) == sc

prop_simplexId :: Ord a => [Simplex a] -> Bool
prop_simplexId s =
  let s' = simplices (fromSimplices s)
   in all (`elem` s') s

prop_insertDimension :: Ord a => Simplex a -> Bool
prop_insertDimension s =
  dimension (fromSimplices [s]) == length s - 1

prop_containedSimplexComplex :: (Show a, Ord a) => Simplex a -> Property
prop_containedSimplexComplex s =
  let sc = simplexComplex s
  in forAll (elements $ faces s) $ \f -> f `contained` sc

instance (Ord a, Arbitrary a) => Arbitrary (Simplex a) where
  arbitrary = fromList <$> (arbitrary `suchThat` ((<10) . length))

instance (Ord a, Arbitrary a) => Arbitrary (SimplicialComplex a) where
  arbitrary = fromSimplices <$> arbitrary

return []
runTests = $quickCheckAll

main :: IO Bool
main = do
  runTests
