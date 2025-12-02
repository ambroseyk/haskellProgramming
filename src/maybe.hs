import Data.Functor.Classes (liftReadListPrec2Default)
-- data Maybe a = Nothing | Just a
-- data Either a b = Left a | Right b

ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n = if even n then Just (n + 2) else Nothing

type Name = String

type Age = Integer

type ValidatePerson a = Either [PersonInvalid] a

data Person = Person Name Age deriving (Show)

data PersonInvalid 
    = NameEmpty
    | AgeTooLow
    deriving (Show, Eq)

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age >= 0 of
    True -> Right age
    False -> Left [AgeTooLow] 

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
    True -> Right name
    False -> Left [NameEmpty]

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson' (Left badName) _ = Left badName
mkPerson' _ (Left badAge) = Left badAge


mkPerson1 :: Name -> Age -> Either PersonInvalid Person
mkPerson1 name age
    | name == "" = Left NameEmpty
    | age < 0 = Left AgeTooLow
    | otherwise = Right (Person name age)


mkPerson0 :: Name -> Age -> Maybe Person 
mkPerson0 name age 
    | name /= "" && age >= 0 = Just (Person name age)
    | otherwise = Nothing

data Example a = Blah | RoofGoats | Woot a



jb = mkPerson "John Browning" 160

bad = mkPerson "" 32
bad2 = mkPerson "blah" 0
bad3 = mkPerson "blah" (-900000)


safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:[]) = Nothing
safeTail (x:xs) = Just xs


     