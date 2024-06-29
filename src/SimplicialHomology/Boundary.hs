module SimplicialHomology.Boundary where

import Data.List
import qualified Data.Matrix as M
import Data.Maybe

data Boundary a = Boundary
  { cell :: a,
    facets :: [a]
  }
  deriving (Show)

boundaryBasis :: (Eq a) => [a] -> [a] -> Boundary a -> Boundary Int
boundaryBasis facets cells (Boundary c fs) = Boundary (get cells c) (if null facets then [] else get facets <$> fs)
  where
    get basis = fromJust . flip elemIndex basis

boundaryMatrix :: (Num a) => Int -> Int -> [Boundary Int] -> M.Matrix a
boundaryMatrix dom codom bs = M.matrix codom dom go
  where
    table = do
      Boundary j is <- bs
      (i, v) <- zip is (cycle [1, -1])
      return ((i, j), v)

    go (i, j) = fromMaybe 0 ((i - 1, j - 1) `lookup` table)
