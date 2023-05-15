module SimplicialHomology.Utilities where

import Data.Foldable
import Control.Applicative

amap :: (Functor t, Foldable t, Alternative f) => (a -> f b) -> t a -> f b
amap f = asum . fmap f

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe (x:xs) = lastMaybe xs <|> Just x

dup :: a -> (a,a)
dup x = (x,x)
