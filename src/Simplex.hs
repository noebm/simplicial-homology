{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Simplex
  (
  Simplex,
  fromList,
  toList,
  emptySimplex,
  zeroSimplex,
  unsafePrepend,
  faces,
  simplexBoundary,
  )
  where

import Data.List
import Data.Foldable (toList)

import Boundary

newtype Simplex a = Simplex [a]
  deriving (Show, Ord, Eq, Functor, Foldable)

-- | Generate Simplex from a list (interpreted as a set).
-- So duplicates get removed and permutations generate the same Simplex.
fromList :: Ord a => [a] -> Simplex a
fromList = Simplex . map head . group . sort

-- | Unsafe. Might lead to inconsistent (read unordered simplices).
unsafePrepend :: a -> Simplex a -> Simplex a
unsafePrepend v (Simplex vs) = Simplex (v : vs)

-- | Simplex consisting of empty set.
emptySimplex :: Simplex a
emptySimplex = Simplex []

-- | Simplex consisting of a single point.
zeroSimplex :: a -> Simplex a
zeroSimplex = Simplex . (:[])

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

-- | All subsimplices contained in Simplex.
faces :: Simplex a -> [Simplex a]
faces (Simplex xs) = Simplex <$> subsets xs

-- | Boundary ordered by index
simplexBoundary :: Simplex a -> Boundary (Simplex a)
simplexBoundary (Simplex xs) = Boundary (Simplex xs) (Simplex <$> go xs) where
  go [] = []
  go (x:xs) = xs: ((x:) <$> go xs)
