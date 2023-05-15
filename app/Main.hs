module Main where

import SimplicialHomology.SimplicialComplex.Examples
import SimplicialHomology

main :: IO ()
main = do
  putStrLn "Homology of RP^2"
  print $ simplicialHomology realP2

  putStrLn "Homology of S^4"
  print $ simplicialHomology $ sphere 4
