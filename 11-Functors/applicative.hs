import System.Directory.Internal.Prelude (Applicative)
-- myAction :: IO String
-- myAction = do
--   a <- getLine
--   b <- getLine
--   return $ a ++ b

myAction :: IO String
myAction = (++) <$> getLine <*> getLine

main = do
  a <- myAction
  putStrLn $ "The tow lines concatenated turn out to be: " ++ a

liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b