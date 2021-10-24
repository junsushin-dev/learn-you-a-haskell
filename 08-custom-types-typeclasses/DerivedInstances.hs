data Person = Person {
  firstName :: String,
  lastName :: String,
  age :: Int
} deriving (Eq, Show, Read)

jiyun = Person { firstName = "Jiyun", lastName = "Jun", age = 29 }
daehyun = Person { firstName = "Daehyun", lastName = "Jang", age = 28 }
junsu = Person { firstName = "Junsu", lastName = "Shin", age = 28 }

surfers = [jiyun, daehyun, junsu]

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)