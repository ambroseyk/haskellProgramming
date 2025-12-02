module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = "OUT_OF_Range_0-9"

digits :: Int -> [Int]
digits n = go n []
  where
    go n ds =
      let (d, m) = divMod n 10
       in if d == 0
            then m : ds
            else go d (m : ds)

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits
