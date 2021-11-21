import Control.Monad.Writer
import Control.Monad
import Data.List

-- liftM :: (Moand m) => (a -> b) -> m a -> m b
-- liftM f m = m >>= (\x -> return (f x))

-- liftM :: (Monad m) => (a -> b) -> m a -> m b
-- liftM f m = do
--   x <- m
--   return (f x)

ap :: (Monad m) => m (a -> b) -> m a -> m b
ap mf m = do
  f <- mf
  x <- m
  return (f x)

join :: (Monad m) => m (m a) -> m a
join mm = do
  m <- mm
  m

joinMaybes :: Maybe Int
joinMaybes = do
  m <- Just (Just 8)
  m

keepSmall :: Int -> Writer [String] Bool
keepSmall x
  | x < 4 = do
    tell ["Keeping " ++ show x]
    return True
  | otherwise = do
    tell [show x ++ " is too large, throwing it away"]
    return False

powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs

binSmalls :: Int -> Int -> Maybe Int 
binSmalls acc x
  | x > 9 = Nothing 
  | otherwise = Just (acc + x)

solveRPN :: String -> Maybe Double 
solveRPN st = do
  [result] <- foldM foldingFunction [] (words st)
  return result

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return ((x * y):ys)
foldingFunction (x:y:ys) "+" = return ((x + y):ys)
foldingFunction (x:y:ys) "-" = return ((y - x):ys)
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x,"")] -> Just x
                                _ -> Nothing
