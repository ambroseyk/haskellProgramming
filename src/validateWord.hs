newtype Word' = Word' String deriving (Eq, Show)

vowels :: String
vowels = "aeiou"

numVowelsConsonants :: String -> (Integer, Integer)
numVowelsConsonants = foldr f (0, 0)
  where
    f :: Char -> (Integer, Integer) -> (Integer, Integer)
    f c (i, n) = if c `elem` vowels then (i + 1, n) else (i, n + 1)

mkWord :: String -> Maybe Word'
mkWord str =
  let (num_vowels, num_consonants) = numVowelsConsonants str
   in if num_vowels > num_consonants then Nothing else Just (Word' str)
