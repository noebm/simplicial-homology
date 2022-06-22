module SmithNormalForm where

import qualified Data.Matrix as M
import qualified Data.Vector as V
import Data.List
import Control.Applicative

import Utilities

diagonalize :: Integral a => Int -> M.Matrix a -> M.Matrix a
diagonalize start m = last $ m: unfoldr (uncurry go) (max 1 (min dim start), m) where
  dim = M.nrows m
  go n mat = do
    let diagAbs (Pivot (t,jt), x) = (\x -> M.setElem (abs $ M.getElem n n x) (n,n) x) $ M.switchCols t jt x
    mat' <- diagAbs <$> diagStep mat n
    return (mat', (n+1, mat'))

diagStep :: Integral a => M.Matrix a -> Int -> Maybe (Pivot, M.Matrix a)
diagStep m t = do
  (p, m') <- setupPivot m t
  (,) p <$> (elimAtPivot p m' <|> pure m')

newtype Pivot = Pivot (Int,Int)
  deriving Show

flipPivot :: Pivot -> Pivot
flipPivot (Pivot (t, jt)) = Pivot (jt, t)

setupPivot :: Integral a => M.Matrix a -> Int -> Maybe (Pivot, M.Matrix a)
setupPivot m t = do
  (t', jt) <- firstNonzero m t
  let m' = if t' /= t then M.switchRows t t' m else m
  let p = Pivot (t, jt)
  return (p, m')

-- first row column pair (t', jt) such that such that jt >= t that has a nonzero entry
-- this might not depend on t but on j_(t - 1)
firstNonzero :: Integral a => M.Matrix a -> Int -> Maybe (Int, Int)
firstNonzero m t = amap go [t..M.ncols m] where
  go jt = do
    t' <- (+ 1) <$> V.findIndex (/= 0) (M.getCol jt m)
    return (t',jt)

-- Makes column entries divisible by pivot & eliminates them
elimColumnAtPivot :: Integral a => Pivot -> M.Matrix a -> Maybe (M.Matrix a)
elimColumnAtPivot (Pivot (t, jt)) mat = lastMaybe $ unfoldr go (mat, nonzeros mat startIndices) where

  startIndices = [t+1..M.nrows mat]
  nonzeros m = filter (\i -> M.unsafeGet i jt m /= 0)

  -- reduce accesses by keeping around the relevant indices
  go (m, indices) = do
    (k, indices') <- uncons indices
    let vPivot = M.unsafeGet t jt m
    let vNonzero = M.unsafeGet k jt m
    let (q, r) = vNonzero `divMod` vPivot
    let m' = M.combineRows k (-q) t m
    let (m'', indices'') = if r /= 0 then (M.switchRows t k m', indices) else (m', indices')
    return (m'', (m'', indices''))

elimRowAtPivot p = fmap M.transpose . elimColumnAtPivot (flipPivot p) . M.transpose

elimAtPivot :: Integral a => Pivot -> M.Matrix a -> Maybe (M.Matrix a)
elimAtPivot p = lastMaybe . unfoldr (fmap dup . step)
  where

  elimColumn = elimColumnAtPivot p
  elimRow = elimRowAtPivot p

  step m = do
    let m' = elimColumn m
    let m'' = elimRow =<< (m' <|> pure m)
    m'' <|> m'

-- tries to repair all divisibilies at once
-- and returns the first relevant index as a hint for diagonalisation step
repairDivisibility :: Integral a => M.Matrix a -> Maybe (Int, M.Matrix a)
repairDivisibility m = do
  (index, _) <- uncons nondiv
  return (index, foldr ($) m (modifier <$> nondiv))
  where mn = min (M.ncols m) (M.nrows m)
        check i =
          let y = M.getElem (i + 1) (i + 1) m
              x = M.getElem i i m
           in x /= 0 && y `rem` x /= 0
        nondiv = filter check [1..mn - 1]
        modifier i mat = let j = i + 1 in M.setElem (M.getElem j j mat) (j, i) mat

smithNormalForm :: Integral a => M.Matrix a -> M.Matrix a
smithNormalForm mat = last $ m0 : unfoldr go m0 where
  m0 = diagonalize 1 mat
  go m = dup . uncurry diagonalize <$> repairDivisibility m

invariantFactors :: Integral a => M.Matrix a -> V.Vector a
invariantFactors = V.filter (/= 0) . M.getDiag . smithNormalForm
