stops = "pbtdkg"

vowels = "aeiou"

nouns = ["car", "bike", "plane", "house", "man", "mountain", "tree", "garden", "fence", "map"]

verbs = ["run", "chase", "poop", "walk", "read", "speak", "write", "listen", "watch"]

tripples xs ys = [(x, y, z) | x <- xs, y <- ys, z <- xs]

f = fromInteger . toInteger

seekritFunc x =
  (/)
    (f (sum (map length (words x))))
    (f (length (words x)))

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

myElem :: (Eq a) => a -> [a] -> Bool
myElem a = myAny (== a)

myElem1 :: (Eq a) => a -> [a] -> Bool
myElem1 a = foldr ((||) . (== a)) False

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter pred =
  let f x xs
        | pred x = x : xs
        | otherwise = xs
   in foldr f []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy comp xs =
  let f x y = case comp x y of
        EQ -> x
        LT -> y
        GT -> x
   in foldr f (last xs) xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy comp xs =
  let f x y = case comp x y of
        EQ -> y
        LT -> x
        GT -> y
   in foldr f (last xs) xs
