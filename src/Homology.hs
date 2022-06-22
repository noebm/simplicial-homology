module Homology where

import qualified Data.Matrix as M
import qualified Data.Vector as V
import Data.Foldable
import Data.Monoid

import ChainComplex
import SmithNormalForm

data HomologyFactors = HomologyFactors { free :: Int, torsion :: V.Vector Int }
  deriving (Show, Eq)

homology :: Integral a => ChainComplex (M.Matrix a) -> [ HomologyFactors ]
homology = quotients . boundaryMaps . fmap invariantFactors where
  quotients xs = zipWith calcQuotient xs (tail xs)

  rank :: LinearMap (V.Vector a) -> Int
  rank = getSum . foldMap (Sum . length)

  calcQuotient b a = HomologyFactors
    (codomainDim a - rank a - rank b)
    (V.map fromIntegral $ V.filter (/= 1) $ fold a)

