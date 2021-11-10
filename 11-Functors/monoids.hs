import Data.Monoid
import qualified Data.Foldable as F
import Distribution.Simple.Utils (xargs)

-- lengthCompare :: String -> String -> Ordering
-- lengthCompare x y = 
--   let a = length x `compare` length y
--       b = x `compare` y
--   in if a == EQ then b else a

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend` 
                    (vowels x `compare` vowels y) `mappend`
                    (x `compare` y)
                    where vowels = length . filter (`elem` "aeiou")

-- instance Monoid a => Monoid (Maybe a) where
--   mempty = Nothing
--   Nothing `mappend` m = m
--   m `mappend` Nothing = m
--   Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

-- newtype First a = First { getFirst :: Maybe a }
--   deriving (Eq, Ord, Read, Show)

-- instance Monoid (First a) where
--   mempty = First Nothing 
--   First (Just x) `mappend` _ = First (Just x)
--   First Nothing `mappend` x = x

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance F.Foldable Tree where
  foldMap f Empty = mempty 
  foldMap f (Node x l r) = F.foldMap f l `mappend`
                           f x           `mappend`
                           F.foldMap f r

testTree = Node 5  
            (Node 3  
                (Node 1 Empty Empty)  
                (Node 6 Empty Empty)  
            )  
            (Node 9  
                (Node 8 Empty Empty)  
                (Node 10 Empty Empty)  
            )