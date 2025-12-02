-- Functor
-- each 
-- a kind is a type of a type constructor
-- (->) is a type constructor. 
-- the input and ouput must be of kind *

-- class Funcotr f where 
--     fmap :: (a -> b) -> f a -> f b
-- --has kind      *    ->  *  ->  *

-- Functor Laws:
-- fmap id == id
-- fmap (f . g) == fmap f . fmap g 

data FixMePls a
    = FixMe 
    | Pls a
    deriving (Eq, Show)

instance Functor FixMePls where 
    fmap _ FixMe = FixMe
    fmap f (Pls a) = Pls (f a)

data WhoCares a 
    = ItDoesnt
    | Matter a 
    | WhatThisIsCalled 
    deriving (Eq,Show)

instance Functor WhoCares where
    fmap :: (a -> b) -> WhoCares a -> WhoCares b
    fmap _ ItDoesnt = WhatThisIsCalled
    fmap f (Matter a) = Matter (f a)
    fmap _ WhatThisIsCalled = ItDoesnt 

data CountingBad a 
    = Heisenberg Int a 
    deriving (Eq, Show)

instance Functor CountingBad where 
    fmap :: (a -> b)  -> CountingBad a -> CountingBad b
    fmap f (Heisenberg n a) = Heisenberg (n+1) (f a)

