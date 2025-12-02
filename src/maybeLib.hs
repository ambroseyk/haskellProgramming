isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b f Nothing = b
mayybee b f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe fb = mayybee fb id

listToMaybe :: [a] -> Maybe a
listToMaybe = foldr (\x _ -> Just x) Nothing

maybeToList :: Maybe a -> [a]
maybeToList = mayybee [] (: [])

-- this was wrong! but, kinda cool
-- catMaybes :: [Maybe a] -> [a]
-- catMaybes = map (fromMaybe "NOthing wAs HerE")

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr f []
  where
    f :: Maybe a -> [a] -> [a]
    f Nothing xs = xs
    f (Just a) xs = a : xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr f (Just [])
  where
    f :: Maybe a -> Maybe [a] -> Maybe [a]
    f _ Nothing = Nothing
    f Nothing _ = Nothing
    f (Just a) (Just as) = Just (a : as)

-- from chatGPT
flipMaybe1 :: [Maybe a] -> Maybe [a]
flipMaybe1 [] = Just []
flipMaybe1 (x : xs) = case x of
  Nothing -> Nothing
  Just y -> case flipMaybe1 xs of
    Nothing -> Nothing
    Just ys -> Just (y : ys)
