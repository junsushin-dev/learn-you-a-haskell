-- instance Functor IO where
--   fmap f action = do
--     result <- action
--     return (f result)

import Data.Char
import Data.List

-- main = do 
--   line <- fmap reverse getLine
--   putStrLn $ "You said " ++ line ++ " backwards!"
--   putStrLn $ "Yes, you really said" ++ line ++ " backwards!"

main = do
  line <- fmap (intersperse '-' . reverse . map toUpper) getLine 
  putStrLn line

data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
  fmap f CNothing = CNothing
  fmap f (CJust counter x) = CJust (counter + 1) (f x)