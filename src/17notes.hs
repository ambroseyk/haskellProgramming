module Notes17 where

import Data.List (elemIndex)
import Control.Applicative


f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]

g y = lookup y [(7, "sup?"), (8, "chris"), (9, "aloha")]

h z = lookup z [(2, 3), (5, 6), (7, 8)]

m x = lookup x [(4, 10), (8, 13), (1, 9001)]

added :: Maybe Integer
added = (+ 3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = pure (,) <*> y <*> z

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = pure max' <*> x <*> y'

xs = [1, 2, 3]

ys = [4, 5, 6]

x' :: Maybe Integer
x' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = pure sum <*> ((,) <$> x' <*> y'')

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure x = Identity x
  (<*>) (Identity f) (Identity x) = Identity (f x)

newtype Constant a b
  = Constant {getConstnat :: a}
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap f (Constant x) = (Constant x)

instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty
    (<*>) (Constant x) (Constant y) = Constant (mappend x y)

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
    if (length s) > maxLen
        then Nothing
        else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq,Show)
newtype Age = Age Integer deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a


data Person = Person Name Address Age
    deriving (Eq, Show)

mkPerson' :: String -> String -> Integer -> Maybe Person
mkPerson' n a a'= Person <$> mkName n <*> mkAddress a <*> (Just (Age a'))

data Cow = Cow {
      name   :: String
    , age    :: Int
    , weight :: Int
   } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty ""  = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              Just (Cow nammy agey weighty)

cowFromString' :: String -> Int -> Int -> Maybe Cow
cowFromString' name' age' weight' =
  Cow <$> noEmpty name' 
      <*> noNegative age' 
      <*> noNegative weight'

cowFromString'' :: String -> Int -> Int -> Maybe Cow 
cowFromString'' name' age' weight' = 
  liftA3 Cow (noEmpty name') 
             (noNegative age') 
             (noNegative weight') 

cowFromString''' :: String -> Int -> Int -> Maybe Cow 
cowFromString''' name' age' weight' = 
  pure Cow <*> noEmpty name' <*> noNegative age' <*> noNegative weight' 


n = pure ($ 2) <*> Just (+2)
-- concretizing Applicatives methods for Maybe 

mPure :: a -> Maybe a 
mPure = pure 

embed :: Num a => Maybe ((a -> b) -> b) 
embed = mPure ($ 2)

mApply :: Maybe ((a -> b) -> b) -> Maybe (a -> b) -> Maybe b 
mApply = (<*>) 

myResult = embed `mApply` Just (+2)