# Simplicial Homology

## Overview

This library calculates the simplicial homology of a given simplicial complex represented as a simplex tree.

The calculation works as follows:
- Calculate boundary maps with signs from simplicial tree
- Generate matrices from boundary maps
- Calculate the Smith normal form
- Calculate Homology groups

## Examples

Here is an example program that calculates the simplicial homology of the real projective plane and the four-sphere.

```haskell
import SimplicialHomology.SimplicialComplex.Examples
import SimplicialHomology

main :: IO ()
main = do
  -- Calculate the homology groups of the real projective plane
  putStrLn "Homology of RP^2"
  print $ simplicialHomology realP2

  -- Calculate the homology groups of the four-sphere
  putStrLn "Homology of S^4"
  print $ simplicialHomology $ sphere 4
```
