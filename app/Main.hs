module Main where

import Lib
import Data.Tree

main :: IO ()
main = do
  putStrLn "Homology of RP^2"
  print $ homology realP2

  putStrLn "Homology of S^4"
  print $ homology $ sphere 4
