module SmithNormalForm where

import qualified Data.Matrix as M
import qualified Data.Vector as V
import Data.Foldable hiding (find)
import Data.List
import Data.Maybe

import Control.Applicative
import Control.Monad

amap :: (Functor t, Foldable t, Alternative f) => (a -> f b) -> t a -> f b
amap f = asum . fmap f

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe (x:xs) = lastMaybe xs <|> Just x

dup :: a -> (a,a)
dup x = (x,x)

-- matrix index with nonzero value
findNonZero :: (Eq a, Num a) => V.Vector a -> Maybe Int
findNonZero = V.findIndex (/= 0)

diagonalize :: Integral a => Int -> M.Matrix a -> M.Matrix a
diagonalize start m = last $ unfoldr (uncurry go) (max 1 (min dim start), m) where
  dim = M.ncols m
  go n mat = do
    guard (n <= dim)
    let diagAbs x = M.setElem (abs $ M.getElem n n x) (n,n) x
    mat' <- diagAbs <$> diagStep mat n <|> pure mat
    return (mat', (n+1, mat'))

diagStep :: Integral a => M.Matrix a -> Int -> Maybe (M.Matrix a)
diagStep m t = do
  (p, m') <- setupPivot m t
  elimAtPivot p m' <|> pure m'

newtype Pivot = Pivot (Int,Int)
  deriving Show

flipPivot :: Pivot -> Pivot
flipPivot (Pivot (t, jt)) = Pivot (jt, t)

setupPivot :: Integral a => M.Matrix a -> Int -> Maybe (Pivot, M.Matrix a)
setupPivot m t = do
  (t', jt) <- pivot m t
  let m' = if t' /= t then M.switchRows t t' m else m
  let p = Pivot (t, jt)
  return (p, m')

-- first row column pair (t', jt) such that such that jt >= t that has a nonzero entry
-- this might not depend on t but on j_(t - 1)
pivot :: Integral a => M.Matrix a -> Int -> Maybe (Int, Int)
pivot m t = amap go [t..M.ncols m] where
  go jt = do
    t' <- (+ 1) <$> findNonZero (M.getCol jt m)
    return (t',jt)

-- Makes column entries divisible by pivot & eliminates them
elimColumnAtPivot :: Integral a => Pivot -> M.Matrix a -> Maybe (M.Matrix a)
elimColumnAtPivot (Pivot (t, jt)) mat = lastMaybe $ unfoldr go (mat, nonzeros mat startIndices) where

  startIndices = [1..t-1] ++ [t+1..M.nrows mat]
  nonzeros m = filter (\i -> M.unsafeGet i jt m /= 0)

  -- reduce accesses by keeping around the relevant indices
  go (m, indices) = do
    k <- amap pure indices
    let vPivot = M.unsafeGet t jt m
    let vNonzero = M.unsafeGet k jt m
    let (q, r) = vNonzero `divMod` vPivot
    let m' = M.combineRows k (-q) t m
    let (m'', indices') = if r /= 0 then (M.switchRows t k m', indices) else (m', tail indices)
    return (m'', (m'', indices'))

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

repairDivisibility :: Integral a => M.Matrix a -> Maybe (Int, M.Matrix a)
repairDivisibility m =
  if null nondiv
     then Nothing
     else Just (head nondiv, foldl' (.) id (modifier <$> nondiv) m)
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
  go m = do
    (index, m') <- repairDivisibility m
    let m'' = diagonalize index m'
    return (m'', m'')

invariantFactors :: Integral a => M.Matrix a -> V.Vector a
invariantFactors = V.filter (/= 0) . M.getDiag . smithNormalForm
