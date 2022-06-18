module Examples where

import SimplicialComplex
import Simplex
import Data.Tree
import Data.List

complete :: Int -> SimplicialComplex Int
complete = SimplicialComplex . go where

  go :: Int -> [ Tree Int ]
  go n = unfoldForest (\l -> (l, [l+1..n])) [0..n]

-- | Triangulation of RP2
-- Calculation currently hangs because of smith normal form implementation
realP2 :: SimplicialComplex Int
realP2 = fromSimplices . fmap fromList $
  [ [0,1,2]
  , [0,2,3]
  , [0,3,4]
  , [0,4,5]
  , [0,1,5]
  , [1,3,5]
  , [1,3,4]
  , [1,2,4]
  , [2,4,5]
  , [2,3,5]
  ]
