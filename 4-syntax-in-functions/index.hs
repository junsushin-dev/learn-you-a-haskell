sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "Can't call head on an empty list!"
head' (x:_) = x

head'' :: [a] -> a
head'' xs = case xs of  [] -> error "No head for empty lists"
                        (x:_) -> x

tell :: (Show a) => [a] -> String
tell [] = "empty list"
tell [x] = show x
tell [x,y] = show x ++ show y
tell (x:y:_) = "long list" ++ show x ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty String!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is" ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "You're underweight!"
  | bmi <= normal = "You're normal!"
  | bmi <= fat = "You're overweight!"
  | otherwise                   = "You're an elephant!"
  where bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b     = a
  | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
myCompare a b
  | a > b     = GT
  | a == b    = EQ
  | otherwise = LT

initials :: String -> String -> String
initials firstName lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstName
        (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in  sideArea + 2 * topArea

describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
  where what [] = "empty."
        what [x] = "a singleton list."
        what xs = "a longer list."