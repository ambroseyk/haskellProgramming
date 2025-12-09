

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FunctorTest where
import Test.QuickCheck
import Test.QuickCheck.Function



-- fmap id = id
-- fmap (p . q) == (fmap p) . (fmap q)
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool 
functorIdentity f = (fmap id f) == f 

functorCompose :: (Eq (f c), Functor f) => 
    (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = 
    (fmap (g . f) x) == ((fmap g . fmap f) $ x)
    -- (fmap (g . f) x) == (fmap g (fmap f x))

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = 
    (fmap (g . f) x) == (fmap g . fmap f $ x)

-- Exercise 
newtype Identity a = Identity a 
    deriving (Eq, Show, Arbitrary) 
instance Functor Identity where 
    fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a 
    deriving (Eq, Show)
instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)
instance Arbitrary a => Arbitrary (Pair a) where 
    arbitrary = do 
        a <- arbitrary
        a1 <- arbitrary 
        return $ Pair a a1

data Two a b = Two a b
    deriving (Eq, Show)
instance Functor (Two a) where 
    fmap f (Two x y) = Two x (f y)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do 
        a <- arbitrary
        b <- arbitrary 
        return $ Two a b 
        
data Three a b c = Three a b c
    deriving (Eq, Show)
instance Functor (Three a b) where 
    fmap f (Three x y z) = Three x y (f z)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where 
    arbitrary = do 
        x <- arbitrary 
        y <- arbitrary 
        z <- arbitrary 
        return $ Three x y z

data Three' a b = Three' a b b
    deriving (Eq, Show)
instance Functor (Three' a) where
    fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) 
  where 
    arbitrary = do 
      x <- arbitrary 
      y <- arbitrary 
      z <- arbitrary 
      return $ Three' x y z

data Four a b c d = Four a b c d
    deriving (Eq, Show)
instance Functor (Four a b c) where 
    fmap f (Four w x y z) = Four w x y (f z)
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => 
                Arbitrary (Four a b c d) where 
    arbitrary = do 
        w <- arbitrary 
        x <- arbitrary 
        y <- arbitrary 
        z <- arbitrary 
        return $ Four w x y z

data Four' a b = Four' a a a b 
    deriving (Eq, Show)
instance Functor (Four' a) where 
    fmap f (Four' w x y z) = Four' w x y (f z)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do 
        w <- arbitrary 
        x <- arbitrary 
        y <- arbitrary 
        z <- arbitrary 
        return $ Four' w x y z 

data Trivial = Trivial 
-- cannot write Functor instance since Functor requires type to be of kind * -> *
-- Trivial has kind *

type IntToChar = Fun Int Char 
type CharToBool = Fun Char Bool 


main :: IO ()
main = do 
    quickCheck (functorIdentity :: Identity Int -> Bool) 
    quickCheck (functorCompose' :: Identity Int -> IntToChar -> CharToBool -> Bool)

    quickCheck (functorIdentity :: Pair Int -> Bool)
    quickCheck (functorCompose' :: Pair Int -> IntToChar -> CharToBool -> Bool)

    quickCheck (functorIdentity :: Two Bool Int -> Bool)
    quickCheck (functorCompose' :: Two Bool Int -> IntToChar -> CharToBool -> Bool)

    quickCheck (functorIdentity :: Three Bool Bool Int -> Bool)
    quickCheck (functorCompose' :: Three Bool Bool Int -> IntToChar -> CharToBool -> Bool)

    quickCheck (functorIdentity :: Three' Float Bool -> Bool)
    quickCheck (functorCompose' :: Three' Float Int -> IntToChar -> CharToBool -> Bool)

    quickCheck (functorIdentity :: Four Integer String Bool Int -> Bool)
    quickCheck (functorCompose' :: Four Integer String Char Int -> IntToChar -> CharToBool -> Bool)
    
    quickCheck (functorIdentity :: Four' String Char -> Bool)
    quickCheck (functorCompose' :: Four' String Int -> IntToChar -> CharToBool -> Bool)


