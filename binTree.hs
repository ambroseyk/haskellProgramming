{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = case f x of
  Nothing -> Leaf
  Just (left, y, right) -> Node (unfold f left) y (unfold f right)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
  where
    f :: Integer -> Maybe (Integer, Integer, Integer)
    f x
      | x == n = Nothing
      | otherwise = Just (x + 1, x, x + 1)
