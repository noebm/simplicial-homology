module ChainComplex where

-- | Bounded chain complex of free groups
newtype ChainComplex a = ChainComplex { boundaryMaps :: [LinearMap a] }
  deriving (Show)

mapChain :: (LinearMap a -> LinearMap b) -> ChainComplex a -> ChainComplex b
mapChain f = ChainComplex . fmap f . boundaryMaps

instance Functor ChainComplex where
  fmap f (ChainComplex d) = ChainComplex (fmap f <$> d)

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

