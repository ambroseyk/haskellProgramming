module Person where 
type Name = String

type Age = Integer

data Person = Person Name Age deriving (Show)

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $
        PersonInvalidUnknown $
          "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do 
    putStrLn "*** Create a person ***"
    putStr "Enter Age: "
    ageStr <- getLine 
    putStr "Enter Name: "
    name <- getLine
    let age = read ageStr :: Integer in 
        case mkPerson name age of 
            Left personInvalid -> do
                putStrLn $ "NOOOO, error occured: " ++ show personInvalid
            Right person -> do 
                putStrLn $ "Yay! Successfully got a person: " ++ show person  
