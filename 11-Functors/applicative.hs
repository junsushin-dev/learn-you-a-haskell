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