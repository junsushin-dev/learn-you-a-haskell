import Data.List
import Data.Char

-- Data.List
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack = 
  let nlen = length needle
  in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)
  
-- Data.Char
encode :: Int -> String -> String
encode shift msg =
  let ords = map ord msg
      shifted = map (+ shift) ords
  in  map chr shifted

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg