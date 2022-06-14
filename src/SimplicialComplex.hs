module SimplicialComplex where

import Data.Tree
import Data.List
import Simplex

newtype SimplicialComplex a = SimplicialComplex (Forest a)
  deriving (Show)

instance Functor SimplicialComplex where
  fmap f (SimplicialComplex xs) = SimplicialComplex $ fmap (fmap f) xs

draw :: Show a => SimplicialComplex a -> String
draw (SimplicialComplex xs) = drawForest $ fmap (fmap show) xs

simplices :: SimplicialComplex a -> [Simplex a]
simplices (SimplicialComplex xs) = go =<< xs where
  go :: Tree a -> [ Simplex a ]
  go = foldTree (\v f -> Simplex [v] : (prepend v <$> concat f))

complete :: Int -> SimplicialComplex Int
complete = SimplicialComplex . go where

  go :: Int -> [ Tree Int ]
  go n = unfoldForest (\l -> (l, [l+1..n])) [0..n]

completeOrd :: Ord a => [a] -> SimplicialComplex a
completeOrd xs = (xs' !!) <$> complete (length xs' - 1)
  where xs' = map head . group . sort $ xs
