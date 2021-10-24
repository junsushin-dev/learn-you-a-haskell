import Control.Monad
import Data.Char
-- main = do 
--   a <- return "hell"
--   b <- return "yeah!"
--   putStrLn $ a ++ " " ++ b

-- main = do
--   print True
--   print 2
--   print "haha"
--   print 3.2
--   print [3,4,3]

-- main = do
--   c <- getChar
--   when (c /= ' ') $ do 
--     putChar c
--     main

-- main = do
--   rs <- sequence [getLine, getLine, getLine]
--   print rs

-- main = forever $ do
--   putStr "Give me some input: "
--   l <- getLine
--   putStrLn $ map toUpper l

main = do
  colors <- forM [1,2,3,4] (\a -> do
    putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
    getLine)
  putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
  mapM putStrLn colors