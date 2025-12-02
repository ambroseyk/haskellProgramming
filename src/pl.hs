module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"

secondSen = "In the forests of the night\n"

thirdSen = "What immortal hand or eye\n"

fourthSen = "Could frame thy fearful symmetry?"

sentences =
  firstSen
    ++ secondSen
    ++ thirdSen
    ++ fourthSen

-- putStrLn sentences -- should print
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?
-- Implement this

splitOn :: String -> Char -> [String]
splitOn [] c = []
splitOn xs c =
  let (line, rest) = (takeWhile (/= c) xs, dropWhile (/= c) xs)
   in line : splitOn (drop 1 rest) c

myLines :: String -> [String]
myLines = (`splitOn` '\n')

myWords :: String -> [String]
myWords = (`splitOn` ' ')

-- What we want 'myLines sentences' to equal
shouldEqual =
  [ "Tyger Tyger, burning bright",
    "In the forests of the night",
    "What immortal hand or eye",
    "Could frame thy fearful symmetry?"
  ]

shouldEqual2 =
  ["Hi", "there", "Chase!", "what", "is", "popping?"]

-- The main function here is a small test
-- to ensure you've written your function
-- correctly.
main :: IO ()
main =
  print $
    "Test myLines:"
      ++ show (myLines sentences == shouldEqual)
      ++ "Test myWords:"
      ++ show (myWords "Hi there Chase! what is popping?" == shouldEqual2)
