-- firstName, lastName, age, height, phoneNumber, FavoriteIceCream
data Person = Person {
  firstName :: String,
  lastName :: String,
  age :: Int,
  height :: Float,
  phoneNumber :: String,
  flavor :: String
} deriving (Show)

guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"

data Car = Car {
  company :: String,
  model :: String,
  year :: Int
} deriving (Show)

car = Car "Ford" "Mustang" 1967

tellCar :: Car -> String
tellCar (Car { company = c , model = m, year = y }) = "This " ++ c ++ " " ++ m ++ " was mad in " ++ show y

stang = Car { company = "Ford" , model = "Mustang", year = 1967 }
