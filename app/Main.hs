module Main where

import Lib
import Data.List (group, sort, find)
import System.IO.Unsafe (unsafePerformIO)

newtype SimplexTree a = SimplexTree [(a,[SimplexTree a])]
  deriving (Show)

instance Functor SimplexTree where
  fmap f (SimplexTree xs) = SimplexTree $ fmap aux xs
    where
    aux (y , ts) = (f y, fmap (fmap f) ts)

newtype Simplex a = Simplex [a]
  deriving (Show, Ord, Eq)

-- insert :: Ord a => Simplex a -> SimplexTree a -> SimplexTree a
-- insert (Simplex []) t = t
-- insert (Simplex (v : vs)) (SimplexTree t) = SimplexTree (as ++ (v, insert (Simplex vs) s) : b)
--   where (as, bs) = break ((>= v) . fst) $ t

simplices :: SimplexTree a -> [ Simplex a ]
simplices (SimplexTree t) = do
  (v, ts) <- t
  (Simplex [v] :) $ do
    Simplex vs <- concat $ simplices <$> ts
    return $ Simplex (v : vs)

-- disconnected simplial 0-complex of size 2
test0 :: SimplexTree Int
test0 = SimplexTree [(0, []), (1, [])]

test1 :: SimplexTree Int
test1 = SimplexTree [(0, [SimplexTree [(1, [])]]), (1, [])]

complete' :: Int -> SimplexTree Int
complete' = go 0 where

  go :: Int -> Int -> SimplexTree Int
  go k n | k < 0 = SimplexTree []
  go k n = SimplexTree $ do
    l <- [k..n]
    return (l, [ go i n | i <- [l+1..n] ])

completeOrd :: Ord a => [a] -> SimplexTree a
completeOrd xs = (xs' !!) <$> complete' (length xs' - 1)
  where xs' = map head . group . sort $ xs

main :: IO ()
main = do
  putStrLn "test0"
  print test0

  putStrLn "test1"
  print test1

  putStrLn "complete 0"
  print (complete' 0)

  putStrLn "complete 1"
  print (complete' 1)

  putStrLn "complete 2"
  print (complete' 2)

  putStrLn "completeOrd \"abc\""
  print (completeOrd "abc")
