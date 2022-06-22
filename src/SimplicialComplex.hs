{-# LANGUAGE BangPatterns, NamedFieldPuns #-}
module SimplicialComplex where

import Data.Tree
import Data.Maybe
import Data.List
import Data.Foldable
import Data.Monoid
import Simplex
import SmithNormalForm

import qualified Data.Matrix as M
import qualified Data.Vector as V

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
fromSimplices = foldr SimplicialComplex.insert emptyComplex

-- TODO: very inefficient use internal (sorted) structure of complex
-- to find it more efficiently
contained :: Eq a => Simplex a -> SimplicialComplex a -> Bool
contained s sc = s `elem` simplices sc

dimension :: Ord a => SimplicialComplex a -> Int
dimension sc = foldSC aux sc - 1 where
  aux xs = if null xs then 0 else 1 + maximum (snd <$> xs)

size :: SimplicialComplex a -> Int
size = foldSC length

{-# INLINE step #-}
step :: SimplicialComplex a -> [(a, SimplicialComplex a)]
step sc = do
  t <- simplexTree sc
  let a = rootLabel t
  return (a, SimplicialComplex $ subForest t)

-- | Fold simplicial complex
foldSC :: ([(a , b)] -> b) -> SimplicialComplex a -> b
foldSC f = go where
  go sc = f $ do
    (x, sc') <- step sc
    return (x , go sc')

-- | Fold simplicial complex with information about current dimension
foldSCdim :: (Int -> [(a , b)] -> b) -> SimplicialComplex a -> b
foldSCdim f = go (-1) where
  go n sc = f n $ do
    (x, sc') <- step sc
    return (x , go (n+1) sc')

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

data Boundary a = Boundary {
  cell :: a,
  facets :: [a]
} deriving (Show)

boundaryBasis :: Eq a => [a] -> [a] -> Boundary a -> Boundary Int
boundaryBasis facets cells (Boundary c fs) = Boundary (get cells c) (if null facets then [] else get facets <$> fs)
  where get basis = fromJust . flip elemIndex basis

-- | Bounded chain complex of free groups
newtype ChainComplex a = ChainComplex { boundaryMaps :: [a] }
  deriving (Show)

instance Functor ChainComplex where
  fmap f (ChainComplex d) = ChainComplex (f <$> d)

data LinearMap a = FiniteMap { from :: Int, to :: Int, repr :: a } | MapToZero Int | MapFromZero Int
  deriving Show

instance Functor LinearMap where
  fmap f (FiniteMap m n x) = FiniteMap m n (f x)
  fmap f (MapToZero n) = MapToZero n
  fmap f (MapFromZero m) = MapFromZero m

instance Foldable LinearMap where
  foldMap f (FiniteMap _ _ x) = f x
  foldMap f _ = mempty

mapWithSize :: (Int -> Int -> a -> b) -> LinearMap a -> LinearMap b
mapWithSize f (FiniteMap dom codom x) = FiniteMap dom codom (f dom codom x)
mapWithSize f (MapToZero dom) = MapToZero dom
mapWithSize f (MapFromZero codom) = MapFromZero codom

domainDim :: LinearMap a -> Int
domainDim (FiniteMap from _ _) = from
domainDim (MapToZero from) = from
domainDim (MapFromZero _) = 0

codomainDim :: LinearMap a -> Int
codomainDim (FiniteMap _ to _) = to
codomainDim (MapToZero _) = 0
codomainDim (MapFromZero to) = to

boundaryChain :: Eq a => SimplicialComplex a -> ChainComplex (LinearMap [Boundary Int])
boundaryChain sc = mkComplex (size sc) . aux $ allCells sc where
  aux cs = do
    (face, cell) <- zip cs (tail cs)
    return $ FiniteMap (length cell) (length face) $ boundaryBasis face cell . boundaryMap <$> cell

  boundaryMap cell = Boundary cell (boundary cell)
  mkComplex c0 ds = ChainComplex $ MapToZero c0 : ds ++ [MapFromZero $ maybe c0 domainDim (lastMaybe ds)]

boundaryMatrix :: Num a => Int -> Int -> [Boundary Int] -> M.Matrix a
boundaryMatrix dom codom bs = M.matrix codom dom go where
  table = do
    Boundary j is <- bs
    (i, v) <- zip is (cycle [1,-1])
    return ((i, j), v)

  go (i,j) = fromMaybe 0 ((i-1,j-1) `lookup` table)

matrixChain :: (Eq a, Num b) => SimplicialComplex a -> ChainComplex (LinearMap (M.Matrix b))
matrixChain = fmap (mapWithSize boundaryMatrix) . boundaryChain

invariantChain :: (Eq a, Integral b) => SimplicialComplex a -> ChainComplex (LinearMap (V.Vector b))
invariantChain = fmap (fmap invariantFactors) . matrixChain

data HomologyFactors = HomologyFactors { free :: Int, torsion :: V.Vector Int }
  deriving (Show, Eq)

homology :: Eq a => SimplicialComplex a -> [ HomologyFactors ]
homology = quotients . boundaryMaps . invariantChain where
  quotients xs = zipWith calcQuotient xs (tail xs)

  rank :: LinearMap (V.Vector a) -> Int
  rank = getSum . foldMap (Sum . length)

  calcQuotient b a = HomologyFactors
    (codomainDim a - rank a - rank b)
    (V.map fromIntegral $ V.filter (/= 1) $ fold a)
