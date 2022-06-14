module Main where

import Lib
import Data.List (group, sort, find)
import Data.Tree

newtype SimplicialComplex a = SimplicialComplex (Forest a)
  deriving (Show)

instance Functor SimplicialComplex where
  fmap f (SimplicialComplex xs) = SimplicialComplex $ fmap (fmap f) xs

draw :: Show a => SimplicialComplex a -> String
draw (SimplicialComplex xs) = drawForest $ fmap (fmap show) xs

newtype Simplex a = Simplex [a]
  deriving (Show, Ord, Eq)

prepend :: a -> Simplex a -> Simplex a
prepend v (Simplex vs) = Simplex (v : vs)

-- insert :: Ord a => Simplex a -> SimplicialComplex a -> SimplicialComplex a
-- insert (Simplex []) t = t
-- insert (Simplex (v : vs)) (SimplicialComplex t) = SimplicialComplex (as ++ (v, insert (Simplex vs) s) : b)
--   where (as, bs) = break ((>= v) . fst) $ t

simplices :: SimplicialComplex a -> [Simplex a]
simplices (SimplicialComplex xs) = go =<< xs where
  go :: Tree a -> [ Simplex a ]
  go = foldTree (\v f -> Simplex [v] : (prepend v <$> concat f))

-- disconnected simplial 0-complex of size 2
test0 :: SimplicialComplex Int
test0 = SimplicialComplex [Node 0 [], Node 1 []]

test1 :: SimplicialComplex Int
test1 = SimplicialComplex [Node 0 [Node 1 []], Node 1 []]

complete :: Int -> SimplicialComplex Int
complete = SimplicialComplex . go where

  go :: Int -> [ Tree Int ]
  go n = unfoldForest (\l -> (l, [l+1..n])) [0..n]

completeOrd :: Ord a => [a] -> SimplicialComplex a
completeOrd xs = (xs' !!) <$> complete (length xs' - 1)
  where xs' = map head . group . sort $ xs

main :: IO ()
main = do
  putStrLn "test0"
  putStr $ draw test0

  putStrLn "test1"
  putStr $ draw test1

  putStrLn "complete 0"
  putStr $ draw (complete 0)

  putStrLn "complete 1"
  putStr $ draw (complete 1)

  putStrLn "complete 2"
  putStr $ draw (complete 2)

  putStrLn "completeOrd \"abc\""
  putStr $ draw (completeOrd "abc")
