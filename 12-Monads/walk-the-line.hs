import Text.XHtml.Transitional (start)
import Data.Bifunctor (Bifunctor(first))
type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right) 
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise = Nothing

x -: f = f x

banana :: Pole -> Maybe Pole
banana _ = Nothing

-- [Without the do notation]
-- 
-- routine :: Maybe Pole  
-- routine =   
--     case Just (0,0) of   
--         Nothing -> Nothing  
--         Just start -> case landLeft 2 start of  
--             Nothing -> Nothing  
--             Just first -> case landRight 2 first of  
--                 Nothing -> Nothing  
--                 Just second -> landLeft 1 second

routine :: Maybe Pole
routine = do
  start <- return (0,0)
  first <- landLeft 2 start
  Nothing
  second <- landRight 2 first
  landLeft 1 second