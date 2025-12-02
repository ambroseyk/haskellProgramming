data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + natToInteger nat

integerToNat :: Integer -> Maybe Nat
integerToNat int
  | int < 0 = Nothing
  | otherwise = Just (toNat int)
  where
    toNat :: Integer -> Nat
    toNat i
      | i == 0 = Zero
      | otherwise = Succ (toNat (i - 1))
