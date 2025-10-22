-- determine_the_type.hs
-- Chapter 5 exercises
{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

a = (* 9) 6

b = head [(0, "doge"), (1, "kitteh")] -- (0, "doge") :: (Num, String)

c = head [(0 :: Integer, "doge"), (1, "kitteh")] -- (0, "doge") :: (Integer, String)

d = if False then True else False -- True :: Bool

e = length [1 .. 5] -- 5 :: Num

f = (length [1 .. 5]) > (length "TACOCAT") -- False :: Bool

functionH :: [a] -> a
functionH (x : _) = x

functionC :: (Eq a, Ord a) => a -> a -> Bool
functionC x y = if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y