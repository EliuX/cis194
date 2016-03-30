module HomeWork7.Homework7Tests where

newtype Product a = Product { getProduct :: a }
                    deriving (Eq, Show, Bounded, Ord)

instance (Num a) => Monoid (Product a) where
  mempty = Product 1
  (Product x) `mappend` (Product y) = Product (x + y)
