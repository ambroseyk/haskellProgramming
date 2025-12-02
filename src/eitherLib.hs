import Data.Either
import Data.Maybe

-- data Either a b = Left a | Right b
-- data Maybe a = Nothing | Just a

-- list recursive version
lefts1 :: [Either a b] -> [a]
lefts1 [] = []
lefts1 (x : xs) = case x of
  Left y -> y : lefts1 xs
  Right _ -> lefts1 xs

-- foldr version
lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where
    f :: Either a b -> [a] -> [a]
    f (Left x) xs = x : xs
    f (Right _) xs = xs

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where
    f :: Either a b -> [b] -> [b]
    f (Left _) bs = bs
    f (Right x) bs = x : bs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' [] = ([], [])
partitionEithers' (x : xs) =
  case x of
    Left a -> (a : as, bs)
    Right b -> (as, b : bs)
  where
    (as, bs) = partitionEithers' xs

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right b) = Just (f b)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' aToC bToC (Left a) = aToC a
either' aToC bToC (Right b) = bToC b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' bToC = either' (\x -> Nothing) (Just . bToC)

