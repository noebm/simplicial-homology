module SimplicialHomology.Homology where

import Data.Foldable
import qualified Data.Matrix as M
import Data.Monoid
import qualified Data.Vector as V
import SimplicialHomology.ChainComplex
import SimplicialHomology.SmithNormalForm

data HomologyFactors = HomologyFactors {free :: Int, torsion :: V.Vector Int}
  deriving (Show, Eq)

newtype Homology = Homology {getFactors :: [HomologyFactors]}
  deriving (Show, Eq)

homology :: (Integral a) => ChainComplex (M.Matrix a) -> Homology
homology = Homology . quotients . boundaryMaps . fmap invariantFactors
  where
    quotients xs = zipWith calcQuotient xs (tail xs)

    rank :: LinearMap (V.Vector a) -> Int
    rank = getSum . foldMap (Sum . length)

    calcQuotient b a =
      HomologyFactors
        (codomainDim a - rank a - rank b)
        (V.map fromIntegral $ V.filter (/= 1) $ fold a)

betti :: Homology -> [Int]
betti = fmap free . getFactors

torsions :: Homology -> [[Int]]
torsions = fmap (V.toList . torsion) . getFactors
