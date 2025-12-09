import Test.HUnit
-- make a list of all contiguous substrings of length n


-- my_fold :: Integer -> Char -> [String] -> [String]
-- my_fold 0 c (x:xs) = x:xs 
-- my_fold n c (x:substrings) = my_fold (n-1) (c:x):substrings

-- substr :: Integer -> String -> [String]
-- substr 0 s = [""]
-- substr n "" = [""]
-- substr n (x:xs) = substr (
--     n-1)  


substr :: Int -> String -> [String]
substr n [] = [""] 
substr n s@(x:xs) 
    | length s >= n = let (first, rest) = (take n s, drop n s) in 
                            first : substr n xs  
    | otherwise = []


firstN :: Integer -> String -> [String] 
firstN n [] = [""]
firstN 0 s = "":[s]
firstN n (x:xs) = let (first:rest) = firstN (n-1) xs in 
    (x:first):rest


test1 = TestCase (assertEqual "firstN 3 \"chase\"" ["cha","se"] (firstN 3 "chase"))
