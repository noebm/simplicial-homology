{-# LANGUAGE TemplateHaskell #-}

import Lib
import Test.QuickCheck
import Data.List

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

prop_Simplex_fromList_permutation_invariant :: (Show a, Ord a) => [a] -> Property
prop_Simplex_fromList_permutation_invariant xs =
  length xs < 10 ==>
    let s = fromList xs
     in forAll (shuffle xs) $ \ys -> fromList ys == s

prop_Simplex_fromList_multiplicity_invariant :: Ord a => [a] -> Property
prop_Simplex_fromList_multiplicity_invariant xs =
    let s = fromList xs
     in length xs < 10 ==> fromList (nub xs) == s

prop_Simplex_fromList_toList_sameset:: Ord a => [a] -> Bool
prop_Simplex_fromList_toList_sameset xs =
  let ys = toList (fromList xs)
  in all (`elem` xs) ys && all (`elem` ys) xs

instance (Ord a, Arbitrary a) => Arbitrary (Simplex a) where
  arbitrary = fromList <$> (arbitrary `suchThat` ((<10) . length))

instance (Ord a, Arbitrary a) => Arbitrary (SimplicialComplex a) where
  arbitrary = fromSimplices <$> arbitrary

return []
runTests = $quickCheckAll

main :: IO Bool
main = do
  runTests
