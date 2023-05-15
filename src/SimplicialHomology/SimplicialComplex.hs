{-# LANGUAGE BangPatterns #-}
module SimplicialHomology.SimplicialComplex where

import Data.Tree
import Data.Maybe
import Data.List
import Data.Foldable
import Data.Monoid
import qualified Data.Matrix as M
import qualified Data.Vector as V

import SimplicialHomology.Simplex
import SimplicialHomology.ChainComplex
import SimplicialHomology.Homology
import SimplicialHomology.Boundary
import SimplicialHomology.Utilities

newtype SimplicialComplex a = SimplicialComplex { simplexTree :: Forest a }
  deriving (Show, Eq)

emptyComplex :: SimplicialComplex a
emptyComplex = SimplicialComplex []

instance Functor SimplicialComplex where
  fmap f (SimplicialComplex xs) = SimplicialComplex $ fmap (fmap f) xs

draw :: Show a => SimplicialComplex a -> String
draw (SimplicialComplex xs) = drawForest $ fmap (fmap show) xs

simplices :: SimplicialComplex a -> [Simplex a]
simplices (SimplicialComplex xs) = emptySimplex : (go =<< xs) where
  go :: Tree a -> [ Simplex a ]
  go = foldTree (\v f -> zeroSimplex v : (unsafePrepend v <$> concat f))

simplexComplex :: Simplex a -> SimplicialComplex a
simplexComplex s = SimplicialComplex $ go (toList s) where
  go [] = []
  go (s0:s) = let !sc = go s in Node s0 sc: sc

insert :: Ord a => Simplex a -> SimplicialComplex a -> SimplicialComplex a
insert s (SimplicialComplex sc) = SimplicialComplex $ go (toList s) sc where

  go :: Ord a => [a] -> Forest a -> Forest a
  go [] sc = sc
  go (s0:s') [] = let !sc = go s' [] in Node s0 sc : sc
  go s@(s0:s') sc@(Node v t0 : sc') =
    case s0 `compare` v of
      LT -> Node s0 (go s' []) : go s' sc
      EQ -> Node v (go s' t0) : go s' sc'
      GT -> head sc : go s sc'

fromSimplices :: Ord a => [Simplex a] -> SimplicialComplex a
fromSimplices = foldr SimplicialHomology.SimplicialComplex.insert emptyComplex

-- TODO: very inefficient use internal (sorted) structure of complex
-- to find it more efficiently
contained :: Eq a => Simplex a -> SimplicialComplex a -> Bool
contained s sc = s `elem` simplices sc

dimension :: SimplicialComplex a -> Int
dimension = foldSCdim dim where
  dim n xs = if null xs then n else maximum (snd <$> xs)

size :: SimplicialComplex a -> Int
size = foldSC length

{-# INLINE step #-}
step :: SimplicialComplex a -> [(a, SimplicialComplex a)]
step sc = do
  t <- simplexTree sc
  let a = rootLabel t
  return (a, SimplicialComplex $ subForest t)

-- | Fold simplicial complex with information about current dimension
foldSCdim :: (Int -> [(a , b)] -> b) -> SimplicialComplex a -> b
foldSCdim f = go (-1) where
  go n sc = f n $ do
    (x, sc') <- step sc
    return (x , go (n+1) sc')

-- | Fold simplicial complex
foldSC :: ([(a , b)] -> b) -> SimplicialComplex a -> b
foldSC f = foldSCdim (const f)

cells :: Int -> SimplicialComplex a -> [Simplex a]
cells n = foldSCdim go where
  go :: Int -> [(a, [Simplex a])] -> [Simplex a]
  go m xs
    -- currently at level n
    | m == n = [ emptySimplex ]
    -- early termination (no simplex of size n found)
    | null xs = []
    | otherwise = do
      (x, simpls) <- xs
      unsafePrepend x <$> simpls

allCells :: SimplicialComplex a -> [[Simplex a]]
allCells sc = takeWhile (not . null) $ flip cells sc <$> [0..]

boundaryChain :: Eq a => SimplicialComplex a -> ChainComplex [Boundary Int]
boundaryChain sc = mkComplex (size sc) . aux $ allCells sc where
  aux cs = do
    (face, cell) <- zip cs (tail cs)
    return $ FiniteMap (length cell) (length face) $ boundaryBasis face cell . simplexBoundary <$> cell

  mkComplex c0 ds = ChainComplex $ MapToZero c0 : ds ++ [MapFromZero $ maybe c0 domainDim (lastMaybe ds)]

matrixChain :: (Eq a, Num b) => SimplicialComplex a -> ChainComplex (M.Matrix b)
matrixChain = mapChain (mapWithSize boundaryMatrix) . boundaryChain

simplicialHomology :: Eq a => SimplicialComplex a -> Homology
simplicialHomology = homology . matrixChain
