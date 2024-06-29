module SimplicialHomology.SimplicialComplex.Examples where

import Data.List
import Data.Tree
import SimplicialHomology.Boundary
import SimplicialHomology.Simplex
import SimplicialHomology.SimplicialComplex

complete :: Int -> SimplicialComplex Int
complete = SimplicialComplex . go
  where
    go :: Int -> [Tree Int]
    go n = unfoldForest (\l -> (l, [l + 1 .. n])) [0 .. n]

-- | Triangulation of RP2
realP2 :: SimplicialComplex Int
realP2 =
  fromSimplices . fmap fromList $
    [ [0, 1, 2],
      [0, 2, 3],
      [0, 3, 4],
      [0, 4, 5],
      [0, 1, 5],
      [1, 3, 5],
      [1, 3, 4],
      [1, 2, 4],
      [2, 4, 5],
      [2, 3, 5]
    ]

sphere :: Int -> SimplicialComplex Int
sphere n = fromSimplices . facets . simplexBoundary . fromList $ [0 .. (n + 1)]

discrete :: Int -> SimplicialComplex Int
discrete n = fromSimplices . fmap fromList $ [[k] | k <- [0 .. n - 1]]
