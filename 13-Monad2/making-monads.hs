{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.Ratio
import Data.List
import Control.Monad (ap)
import Data.Function

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show

instance Functor Prob where
  fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x, p)) xs

thisSituation :: Prob (Prob Char)
thisSituation = Prob
  [
    (Prob [('a',1%2),('b',1%2)], 1%4),
    (Prob [('c',1%2),('d',1%2)], 3%4)
  ]

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
  where multAll (Prob innerxs, p) = map (\(x,r) -> (x,p*r)) innerxs

instance Applicative Prob where
  pure = return
  (<*>) = ap

instance Applicative Prob => Monad Prob where
  return x = Prob [(x,1%1)]
  m >>= f = flatten (fmap f m)

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads,1%2),(Tails,1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]

flipThree :: Prob Bool 
flipThree = do
  a <- coin
  b <- coin
  c <- loadedCoin
  return (all (==Tails) [a,b,c])

runProb :: Eq a => Prob a -> [(a,Rational)]
runProb = map (\x -> (fst (head x), sum (map snd x)))
          . groupBy ((==) `on` fst)
          . getProb