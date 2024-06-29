module SimplicialHomology.Utilities where

import Control.Applicative
import Data.Foldable

amap :: (Functor t, Foldable t, Alternative f) => (a -> f b) -> t a -> f b
amap f = asum . fmap f

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe (x : xs) = lastMaybe xs <|> Just x

dup :: a -> (a, a)
dup x = (x, x)
