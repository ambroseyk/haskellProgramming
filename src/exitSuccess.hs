import Control.Monad (forever)
import Data.Char (toLower, isAlpha)
import System.Exit (exitSuccess)

example :: String
example = "Madam I'm Adam"

process :: String -> String 
process str = filter isAlpha (map toLower str)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let line2 = process line1
   in case (line2 == reverse line2) of
        True -> putStrLn "It's a palindrome!"
        False -> do
          putStrLn "Nope!"
          exitSuccess