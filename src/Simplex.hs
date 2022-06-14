module Simplex where

newtype Simplex a = Simplex [a]
  deriving (Show, Ord, Eq)

prepend :: a -> Simplex a -> Simplex a
prepend v (Simplex vs) = Simplex (v : vs)
