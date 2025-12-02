-- reverse.hs
module Reverse where

rvrs :: String -> String
rvrs s = let a = (drop 9 s); b = take 2 $ drop 6 s; c = take 5 s 
    in a ++ " " ++ b ++ " " ++ c 

main :: IO ()
main = print $ rvrs "Curry is awesome"


a s = s ++ "!"

b s = s !! 4

c s = drop 9 s

thirdLetter :: String -> Char
thirdLetter s = s !! 2

letterIndex :: Int -> Char
letterIndex i = s !! i where s = "ChasieWasie"