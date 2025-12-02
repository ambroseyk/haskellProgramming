-- Maybe a = Nothing | Just a
notThe :: String -> Maybe String
notThe word
  | word == "the" = Nothing
  | otherwise = Just word

replaceThe :: String -> String
replaceThe str =
  let l = map notThe (words str)
   in unwords (map f l)
  where
    f Nothing = "a"
    f (Just s) = s



isVowel :: Char -> Bool
isVowel = (`elem` ['a', 'e', 'i', 'o', 'u'])

vowelStart :: String -> Bool
vowelStart [] = False
vowelStart (x:_) = isVowel x

isThe :: String -> Bool
isThe = (== "the")

myConcat :: String -> (String, Integer) -> (String, Integer)
myConcat first (rest, i) 
    | isThe first && vowelStart rest = (first ++ rest, i + 1)
    | otherwise = (first ++ rest, i)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = let (rest, i) = foldr myConcat ("", 0) (words str) in i

smush :: Char -> Integer -> Integer
smush c i = if isVowel c then i + 1 else i

countVowels :: String -> Integer
countVowels word = foldr smush 0 word


