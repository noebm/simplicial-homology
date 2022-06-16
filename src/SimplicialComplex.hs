{-# LANGUAGE BangPatterns #-}
module SimplicialComplex where

import Data.Tree
import Data.List
import Simplex

newtype SimplicialComplex a = SimplicialComplex (Forest a)
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

complete :: Int -> SimplicialComplex Int
complete = SimplicialComplex . go where

  go :: Int -> [ Tree Int ]
  go n = unfoldForest (\l -> (l, [l+1..n])) [0..n]

completeOrd :: Ord a => [a] -> SimplicialComplex a
completeOrd xs = (xs' !!) <$> complete (length xs' - 1)
  where xs' = map head . group . sort $ xs

-- TODO: very inefficient use internal (sorted) structure of complex
-- to find it more efficiently
contained :: Eq a => Simplex a -> SimplicialComplex a -> Bool
contained s sc = s `elem` simplices sc

-- TODO: efficient implementation
dimension :: Ord a => SimplicialComplex a -> Int
dimension (SimplicialComplex xs) = (\x -> x - 1) $
  if null xs
     then 0
     else maximum $ length . levels <$> xs
