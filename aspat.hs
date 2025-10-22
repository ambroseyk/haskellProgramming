import Data.Char
import Data.List (intersperse)

f :: (Show a) => (a, b) -> IO (a, b)
f t@(a, _) = do
  print a
  return t

doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x : _) = x : xs

isSubsequenceOf1 :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf1 xs ys = foldr (\x b -> b && (x `elem` ys)) True xs

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf xss@(x : xs) (y : ys)
  | x == y = isSubsequenceOf xs ys
  | otherwise = isSubsequenceOf xss ys

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord w@(x : xs) = toUpper x : xs

capitalizeWords :: String -> [(String, String)]
capitalizeWords xs = map capitalizeWord (words xs)
  where
    capitalizeWord w@(x : xs) = (w, toUpper x : xs)

sentences :: String -> [String]
sentences [] = []
sentences xs =
  let (first, rest) = span (/= '.') xs
   in (first ++ ".") : sentences (drop 2 rest)

capitalizeParagraph :: String -> String
capitalizeParagraph = concat . intersperse " " . map capitalizeWord . sentences
