module Cipher where

import Data.Char

shiftBy :: Int -> Char -> Char
shiftBy x c
  | c `elem` ['A' .. 'Z'] =
      let relOrd = ord c - ord 'A'
       in chr (ord 'A' + mod (relOrd + x) 26)
  | otherwise = c

-- caesarCiph :: String -> String
caesarCiph = do 
  putStr "Enter message(uppercase): "
  msg <- getLine
  putStrLn $ map (shiftBy 1) msg

unCaesar :: String -> String
unCaesar = map (shiftBy (-1))

kwString :: String -> Int -> String -> String
kwString kw i [] = []
kwString kw i (x : xs)
  | x `elem` ['A' .. 'Z'] =
      let relIndex = mod i (length kw)
       in kw !! relIndex : kwString kw (i + 1) xs
  | otherwise = x : kwString kw i xs

kw :: String
kw = "ALLY"

msg :: String
msg = "MEET AT DAWN"

testVigCiph :: IO ()
testVigCiph = do
  print msg
  print (kwString kw 0 msg)
  print (vigCiph kw msg)

vigCiphIn :: IO ()
vigCiphIn = do
  putStrLn "Enter message(uppercase): "
  msg <- getLine 
  putStrLn "Enter Keyword(uppercase): "
  kw <- getLine
  putStrLn $ vigCiph kw msg 

vigCiph :: String -> String -> String
vigCiph kw msg = go kw 0 msg
  where
    go kw i [] = []
    go kw i (x : xs)
      | x `elem` ['A' .. 'Z'] =
          let reli = i `mod` length kw
              kwCharRelOrd = ord (kw !! reli) - ord 'A'
              msgCharRelOrd = ord x - ord 'A'
              relOrd = ((msgCharRelOrd + kwCharRelOrd) `mod` 26)
           in chr (ord 'A' + relOrd) : go kw (i + 1) xs
      | otherwise = x : go kw i xs

vigCiph0 :: String -> String -> String
vigCiph0 kw msg =
  let kwStr = kwString kw 0 msg
   in go msg kwStr
  where
    go [] [] = []
    go (x : xs) (y : ys)
      | x `elem` ['A' .. 'Z'] = shiftBy (ord y - ord 'A') x : go xs ys
      | otherwise = x : go xs ys
