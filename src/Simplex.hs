{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Simplex where

import Data.List

newtype Simplex a = Simplex [a]
  deriving (Show, Ord, Eq, Functor, Foldable)

fromList :: Ord a => [a] -> Simplex a
fromList = Simplex . map head . group . sort

toList :: Simplex a -> [a]
toList (Simplex xs) = xs

prepend :: a -> Simplex a -> Simplex a
prepend v (Simplex vs) = Simplex (v : vs)
