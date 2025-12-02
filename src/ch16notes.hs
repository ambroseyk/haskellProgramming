{-# LANGUAGE RankNTypes #-}
incIfJust :: Num a => Maybe a -> Maybe a 
incIfJust (Just x) = Just (x + 1)
incIfJust Nothing = Nothing 

showIfJust :: Show a => Maybe a -> Maybe String 
showIfJust (Just s) = Just $ show s 
showIfJust Nothing = Nothing 

-- can be simplified using fmap, focusing on the Just case

liftedInc :: (Functor f, Num a) => f a -> f a 
liftedInc = fmap (+ 1)

liftedShow :: (Functor f, Show a) => f a -> f String 
liftedShow = fmap show 

-- Exercise: Possibly (rewrite of Maybe)

data Possibly a
    = LolNope
    | Yeppers a 
    deriving (Eq, Show)

instance Functor Possibly where 
    fmap f (Yeppers x) = Yeppers $ f x 
    fmap _ LolNope = LolNope 

incIfRight :: Num a => Either e a -> Either e a 
incIfRight (Right n) = Right $ n + 1 
incIfRight (Left e) = (Left e)

showIfRight :: Show a => Either e a -> Either e String 
showIfRight (Right s) = Right $ show s 
showIfRight (Left e) = Left e 

-- Sum ~ Either
data Sum a b 
    = First a 
    | Second b 
    deriving (Eq, Show)

instance Functor (Sum a) where 
    fmap _ (First a) = First a 
    fmap f (Second x) = Second $ f x


data Wrap f a = Wrap (f a) deriving (Eq,Show)

instance Functor f => Functor (Wrap f) where 
    fmap f (Wrap fa) = Wrap $ fmap f fa

-- getLine :: IO String
-- read :: Read a => String -> a 
getInt :: IO Int 
getInt = fmap read getLine 

meTooIsm :: IO String
meTooIsm = do 
    input <- getLine 
    return (input ++ "and me too!")

bumpIt :: IO Int 
bumpIt = do 
    intVal <- getInt 
    return (intVal + 1)



-- Nat ~ Natural Transformation
type Nat f g = forall a. f a -> g a

maybeToList :: Nat Maybe []
maybeToList (Just x) = [x]
maybeToLIst Nothing = []

degenerateMtl :: Num a => Nat Maybe [] 
degenerateMtl Nothing = []
degenerateMtl (Just a) = [a + 1]