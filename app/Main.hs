module Main where

import Lib
import Data.List (group, sort, find)
import Data.Tree

newtype SimplexTree a = SimplexTree (Forest a)
  deriving (Show)

instance Functor SimplexTree where
  fmap f (SimplexTree xs) = SimplexTree $ fmap (fmap f) xs

draw :: Show a => SimplexTree a -> String
draw (SimplexTree xs) = drawForest $ fmap (fmap show) xs

newtype Simplex a = Simplex [a]
  deriving (Show, Ord, Eq)

prepend :: a -> Simplex a -> Simplex a
prepend v (Simplex vs) = Simplex (v : vs)

-- insert :: Ord a => Simplex a -> SimplexTree a -> SimplexTree a
-- insert (Simplex []) t = t
-- insert (Simplex (v : vs)) (SimplexTree t) = SimplexTree (as ++ (v, insert (Simplex vs) s) : b)
--   where (as, bs) = break ((>= v) . fst) $ t

simplices :: SimplexTree a -> [Simplex a]
simplices (SimplexTree xs) = go =<< xs where
  go :: Tree a -> [ Simplex a ]
  go = foldTree (\v f -> Simplex [v] : (prepend v <$> concat f))


-- disconnected simplial 0-complex of size 2
test0 :: SimplexTree Int
test0 = SimplexTree [Node 0 [], Node 1 []]

test1 :: SimplexTree Int
test1 = SimplexTree [Node 0 [Node 1 []], Node 1 []]

complete :: Int -> SimplexTree Int
complete = SimplexTree . go where

  go :: Int -> [ Tree Int ]
  go n = unfoldForest (\l -> (l, [l+1..n])) [0..n]

completeOrd :: Ord a => [a] -> SimplexTree a
completeOrd xs = (xs' !!) <$> complete (length xs' - 1)
  where xs' = map head . group . sort $ xs

main :: IO ()
main = do
  putStrLn "test0"
  print test0

  putStrLn "test1"
  print test1

  putStrLn "complete 0"
  print (complete 0)

  putStrLn "complete 1"
  print (complete 1)

  putStrLn "complete 2"
  print (complete 2)

  putStrLn "completeOrd \"abc\""
  print (completeOrd "abc")
