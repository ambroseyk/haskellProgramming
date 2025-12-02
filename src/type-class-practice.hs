import Data.Sequence (adjust')

-- type-class-practice.hs

data DayOfWeek
  = Mon
  | Tue
  | Weds
  | Thu
  | Fri
  | Sat
  | Sun
  deriving (Show)

instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _ = GT
  compare _ Fri = LT
  compare _ _ = EQ

data Date = Date DayOfWeek Int deriving (Show)

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Weds Weds = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

instance Eq Date where
  (==)
    (Date weekday dayOfMonth)
    (Date weekday' dayOfMonth') = weekday == weekday' && dayOfMonth == dayOfMonth'

check' :: (Ord a) => a -> a -> Bool
check' a a' = a == a'

data Mood = Blah

instance Show Mood where
  show _ = "Blah"

class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer
  defaultNumber :: a

-- pretend newtype is data for now
newtype Age
  = Age Integer
  deriving (Eq, Show)

instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n
  defaultNumber = Age 65


newtype Year -- type constructor
  = Year Integer -- data constructor
  deriving (Eq, Show)

instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n
  defaultNumber = Year 1963

sumNumberish :: (Numberish a) => a -> a -> a
sumNumberish a a' = fromNumber summed
  where
    integerOfA = toNumber a
    integerOfAPrim = toNumber a'
    summed = integerOfA + integerOfAPrim


add :: Num a =>  a -> a -> a
add x y = x + y

addWeird :: (Num a, Ord a)i => a -> a -> a
addWeird x y =
  if x > 1
  then x + y
  else x


