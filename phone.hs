import Data.Char
import Data.List

type Digit = Char

type Presses = Int

data DaPhone = DaPhone [(Digit, String)]

convo :: [String]
convo =
  [ "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"
  ]

myPhone =
  DaPhone
    [ ('1', "1"),
      ('2', "abc2"),
      ('3', "def3"),
      ('4', "ghi4"),
      ('5', "jkl5"),
      ('6', "mno6"),
      ('7', "pqrs7"),
      ('8', "tuv8"),
      ('9', "wxyz9"),
      ('*', "^"),
      ('0', " +_0"),
      ('#', ".,")
    ]

-- finds how many presses are need for a character
-- efectivly the index of given char
presses :: String -> Char -> Presses -> Presses
presses [] _ _ = 0
presses (x : xs) c p
  | c == x = p + 1
  | otherwise = presses xs c (p + 1)

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone@(DaPhone ps) c
  | c `elem` ['A' .. 'Z'] = ('*', 1) : reverseTaps phone (toLower c)
  | otherwise =
      let (d, chrs) = head (filter (\(d, chrs) -> c `elem` chrs) ps)
       in [(d, presses chrs c 0)]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone = concatMap (reverseTaps phone)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(_, n) p -> n + p) 0

-- count :: Char -> String -> (Char, Int)
count c xs = (c, foldr go 0 xs)
  where
    go x n = if x == c then n + 1 else n

myCompare (c1, n1) (c2, n2) = compare n1 n2

-- mostPopularLetter :: String -> [(Char, Int)]
mostPopularLetter xs =
  let ys = maximumBy myCompare (map (`count` xs) xs)
      (c, n) = ys
   in c

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

-- coolestWord :: [String] -> String
coolestWord xs =
  let ws = words (concat (intersperse " " xs))
      ys = maximumBy myCompare (map (`count` ws) ws)
      (s, n) = ys
   in s
