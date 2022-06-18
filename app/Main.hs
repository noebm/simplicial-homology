module Main where

import Lib
import Data.Tree

-- disconnected simplial 0-complex of size 2
test0 :: SimplicialComplex Int
test0 = SimplicialComplex [Node 0 [], Node 1 []]

test1 :: SimplicialComplex Int
test1 = SimplicialComplex [Node 0 [Node 1 []], Node 1 []]

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

  print $ homology realP2
