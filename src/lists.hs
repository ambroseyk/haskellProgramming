import Data.Char

eftBool :: Bool -> Bool -> [Bool]
eftBool False True = [False, True]
eftBool False False = [False]
eftBool True True = [True]
eftBool True False = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT LT = [LT]
eftOrd LT EQ = [LT, EQ]
eftOrd LT GT = [LT, EQ, GT]
eftOrd EQ LT = []
eftOrd EQ EQ = [EQ]
eftOrd EQ GT = [EQ, GT]
eftOrd GT LT = []
eftOrd GT EQ = []
eftOrd GT GT = [GT]

eftInt :: Int -> Int -> [Int]
eftInt x y
  | x > y = []
  | x == y = [x]
  | x < y = x : eftInt (succ x) y

eftChar :: Char -> Char -> [Char]
eftChar x y
  | x > y = []
  | x == y = [x]
  | x < y = x : eftChar (succ x) y

myWords :: String -> [String]
myWords [] = []
myWords xs =
  let (first, rest) = (takeWhile (/= ' ') xs, dropWhile (/= ' ') xs)
   in first : myWords (drop 1 rest)

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x : xs) (y : ys) = (x, y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f [] _ = []
myZipWith f _ [] = []
myZipWith f (x : xs) (y : ys) = f x y : myZipWith f xs ys

myOr :: [Bool] -> Bool
myOr [] = False
myOr (b : bs) = if b == True then True else myOr bs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (b : bs) = if f b then True else myAny f bs

myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem e (x : xs) = if e == x then True else myElem e xs

myElem1 :: (Eq a) => a -> [a] -> Bool
myElem1 e = myAny (== e)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x : xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x : xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy comp (x : xs) = case comp x (myMaximumBy comp xs) of
  EQ -> x
  GT -> x
  LT -> myMaximumBy comp xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x] = x
myMinimumBy comp (x : xs) = case comp x (myMinimumBy comp xs) of
  EQ -> x
  GT -> myMinimumBy comp xs
  LT -> x

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare