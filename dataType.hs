data Doggies a
  = Husky a
  | Mastiff a
  deriving (Eq, Show)

data DogueDeBordeaux doge = DogueDeBordeaux doge

data Price
  = Price Integer
  deriving (Eq, Show)

data Manufacturer
  = Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline
  = PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

type Size = Integer

data Vehicle
  = Car Manufacturer Price
  | Plane Airline Size
  deriving (Eq, Show)

myCar = Car Mini (Price 14000)

urCar = Car Mazda (Price 20000)

clownCar = Car Tata (Price 7000)

doge = Plane PapuAir 3000

vehicles = [myCar, urCar, clownCar, doge]

isCar :: Vehicle -> Bool
isCar v = case v of
  Car _ _ -> True
  Plane _ _ -> False

isPlane :: Vehicle -> Bool
isPlane v = case v of
  Car _ _ -> False
  Plane _ _ -> True

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu v = case v of
  Car m p -> m
  Plane a s -> undefined

data Example0
  = Example0
  deriving (Eq, Show)

data Example1
  = Example1 Int
  deriving (Eq, Show)

data Example2 = Example2 Int String deriving (Eq, Show)

data MyType = MyVal Int deriving (Eq, Show)

data Example = MakeExample Int deriving (Show)

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

instance TooMany (Int, String) where
  tooMany :: (Int, String) -> Bool
  tooMany (n, _) = tooMany n

instance TooMany (Int, Int) where
  tooMany :: (Int, Int) -> Bool
  tooMany (x, y) = tooMany (x + y)

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany (x + y)
