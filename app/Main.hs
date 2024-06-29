module Main where

import SimplicialHomology
import SimplicialHomology.SimplicialComplex.Examples

main :: IO ()
main = do
  putStrLn "Homology of RP^2"
  print $ simplicialHomology realP2

  putStrLn "Homology of S^4"
  print $ simplicialHomology $ sphere 4
