{-# LANGUAGE TemplateHaskell #-}

import Lib
import Test.QuickCheck
import Data.List
import qualified Data.Matrix as M
import Data.Maybe
import Control.Applicative

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

prop_all_cells_are_all_simplices sc =
  sort (flip cells sc =<< [-1..dimension sc]) == sort (simplices sc)

instance (Ord a, Arbitrary a) => Arbitrary (Simplex a) where
  arbitrary = fromList <$> (arbitrary `suchThat` ((<10) . length))

instance (Ord a, Arbitrary a) => Arbitrary (SimplicialComplex a) where
  arbitrary = fromSimplices <$> arbitrary

instance Arbitrary a => Arbitrary (M.Matrix a) where
  arbitrary = do
    n <- choose (1, 15)
    m <- choose (1, 15)
    M.fromList n m <$> vector (n*m)

isDiagonal :: (Eq a, Num a) => M.Matrix a -> Bool
isDiagonal m = all (== 0) [ M.unsafeGet i j m | i <- [1..M.nrows m], j <- [1..M.ncols m], i /= j]

prop_diagonalize_isDiagonal m = isDiagonal $ diagonalize 1 m

isEmptyNonPivotRow m (Pivot (t, jt)) = all (== 0) [M.getElem t j m | j <- [1..M.ncols m], j /= jt]
isEmptyNonPivotCol m (Pivot (t, jt)) = all (== 0) [M.getElem i jt m | i <- [1..M.nrows m], i /= t]

prop_nonzero_pivot m = any (/= 0) m ==>
  forAll (elements [1..M.ncols m]) $ isJust . pivot m

prop_elimColumnAtPivot_nondiagonal_zero mat =
  any (/= 0) mat ==>
    let Just (p, m) = setupPivot mat 1
        m' = fromMaybe m (elimColumnAtPivot p m)
     in isEmptyNonPivotCol m' p

prop_elimRowAtPivot_nondiagonal_zero mat =
  any (/= 0) mat ==>
    let Just (p, m) = setupPivot mat 1
        m' = fromMaybe m (elimRowAtPivot p m)
     in isEmptyNonPivotRow m' p

prop_elimAtPivot_nondiagonal_zero mat =
  any (/= 0) mat ==> isEmptyNonPivotRow m' p && isEmptyNonPivotCol m' p
  where Just (p, m) = setupPivot mat 1
        m' = fromMaybe m (elimAtPivot p m)

prop_homology_of_spheres = once $
  forAll (elements [1..6]) $ \n ->
    let h = homology $ sphere n
     in all (null . torsion) h && ([1] ++ replicate (n - 1) 0 ++ [1] == (free <$> h))

return []
runTests = $quickCheckAll

main :: IO Bool
main = do
  runTests
