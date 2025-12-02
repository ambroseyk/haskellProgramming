module Arith2 where 

sumTo :: (Eq a, Num a) => a -> a
sumTo 0 = 0
sumTo n = n + sumTo (n - 1)

myMult :: (Integral a) => a -> a -> a
myMult 0 _ = 0
myMult _ 0 = 0
myMult 1 n = n
myMult c n = n + myMult (c - 1) n

data DividedResult
  = Result Integer
  | DividedByZero
  deriving (Show)

dividedBy :: (Integral a) => a -> a -> DividedResult
dividedBy _ 0 = DividedByZero
dividedBy num denom = go num denom 0
  where
    go n d count
      | (n < 0 && d > 0) || (n > 0 && d < 0) =
          case go (abs n) (abs d) 0 of
            Result q -> Result (negate q)
            DividedByZero -> DividedByZero
      | n < 0 && d < 0 = go (abs n) (abs d) 0
      | n < d = Result count
      | otherwise = go (n - d) d (count + 1)

mc91 :: Integer -> Integer
mc91 n
  | n > 100 = n - 10
  | n <= 100 = mc91 . mc91 $ n + 11
