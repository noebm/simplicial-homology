{-# LANGUAGE BangPatterns, NamedFieldPuns #-}
module SimplicialComplex where

import Data.Tree
import Data.Maybe
import Data.List
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
allCells sc = takeWhile (not . null) $ flip cells sc <$> [(-1)..]

data Boundary a = Boundary {
  cell :: a,
  facets :: [a]
} deriving (Show)

boundaryBasis :: Eq a => [a] -> [a] -> Boundary a -> Boundary Int
boundaryBasis facets cells (Boundary c fs) = Boundary (get cells c) (if null facets then [] else get facets <$> fs)
  where get basis = fromJust . flip elemIndex basis

type AssocMatrix a = (Int, Int, [((Int, Int), a)])

assocMatrix :: Num a => AssocMatrix a -> (Int, Int, [ a ])
assocMatrix (m, n, xs) =
  let ys = sortBy (\(pos,_) (pos',_) -> pos `compare` pos') xs
      go (i,j) ((i',j'),v) = let k = (i' - i) * n + (j' - j) -1
                            in ((i',j'), replicate k 0 ++ [v])
   in (,,) m n $ concat $ snd $ mapAccumL go (0,0) ys

-- | Bounded chain complex of free groups
-- chainBoundaries n should be a map from R ^ chainDimensions (n + 1) to R ^ chainDimensions n.
data ChainComplex a = ChainComplex
  { chainDimensions :: [Int]
  , chainBoundaries :: [a]
  } deriving (Show)

instance Functor ChainComplex where
  fmap f (ChainComplex dims d) = ChainComplex dims (f <$> d)

data LinearMap a = FiniteMap { from :: Int, to :: Int, repr :: a } | MapToZero Int | MapFromZero Int
  deriving Show

incidences :: (Eq a, Num b) => SimplicialComplex a -> ChainComplex (AssocMatrix b)
incidences sc = mkComplex $ go $ allCells sc where
  boundaryMap cell = Boundary cell (boundary cell)

  aux face cell = do
    Boundary j is <- boundaryBasis face cell . boundaryMap <$> cell
    (i, v)  <- zip is (cycle [1, -1])
    return ((i, j), v)

  go list = do
    (face, cell) <- zip list (tail list)
    return $ (,,) (length face) (length cell) $ aux face cell

  mkComplex [] = ChainComplex [] []
  mkComplex xs =
    let dims = fmap (\(_,m,_) -> m) xs
        ys = (0, head dims, []):tail xs ++ [(last dims, 0, [])]
    in ChainComplex (0: dims ++ [0]) ys

boundaries :: Eq a => SimplicialComplex a -> ChainComplex (LinearMap (M.Matrix Integer))
boundaries = fmap toMatrix . incidences where
  -- most likely terrifingly slow!
  toMatrix (m, n, assoc)
    | m == 0 = MapToZero n
    | n == 0 = MapFromZero m
    | otherwise = FiniteMap n m $ M.matrix m n $ \(i,j) -> fromMaybe 0 ((i-1,j-1) `lookup` assoc)

data HomologyFactors = HomologyFactors { free :: Int, torsion :: V.Vector Int }
  deriving (Show, Eq)

homology :: Eq a => SimplicialComplex a -> [ HomologyFactors ]
homology sc = go . chainBoundaries $ smith where
  -- information about image and domain dimensionality and rank
  smith = fmap extract . boundaries $ sc

  extract linmap =
    case linmap of
      FiniteMap {from, to, repr} -> (M.nrows repr, M.ncols repr, invariantFactors repr)
      MapFromZero to -> (to, 0, V.empty)
      MapToZero from -> (0, from, V.empty)

  go xs = zipWith calcQuotient xs (tail xs)
  calcQuotient (_,_, rb) (dim, _, ra) = HomologyFactors (dim - V.length ra - V.length rb)
    (V.map fromIntegral $ V.filter (/= 1) ra)
