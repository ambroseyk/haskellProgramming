
import GHC.Arr

data Bool = False | True

-- No, Bool has kind * where a Functor requires kind * -> *

data BoolAndSomethingElse a = 
    False' a | True' a 

-- Yes, has kind * -> * 

data BoolAndMaybeSomethingElse a = 
    Falsish | Truish a 

-- yup, ibid

newtype Mu f = Inf { outF :: f (Mu f) }

-- yes Mu has kind * -> * 

data D = D (Array Word Word) Int Int 

-- No, D has Kind *, it does not have a type parameter

data Sum b a = First a | Second b 

instance Functor (Sum e) where 
    fmap f (First a) = First (f a)
    fmap f (Second b) = Second b 

data Company a c b = DeepBlue a c | Something b 

instance Functor (Company e e') where 
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c 

data More b a = L a b a | R b a b 
    deriving (Eq, Show)

instance Functor (More x) where 
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b' 

data Quant a b = Finance | Desk a | Bloor b 

instance Functor (Quant a) where
    fmap _ Finance = Finance 
    fmap _ (Desk x) = Desk x 
    fmap f (Bloor x) = Bloor $ f x 

-- data K a b = K a 

-- instance Functor (K a) where 
--     fmap _ (K a) = K a

newtype Flip f a b = Flip (f b a)
    deriving (Eq, Show)

newtype K a b = K a

instance Functor (Flip K a) where 
    fmap f (Flip (K x))  = Flip (K (f x))

data EvilGoateeConst a b = GoatyConst b 

instance Functor (EvilGoateeConst a) where 
    fmap f (GoatyConst x) = GoatyConst $ f x 

data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where 
    fmap f (LiftItOut fa) = LiftItOut $ fmap f fa  


data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where 
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga) 

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where 
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb) 

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where 
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

data List a = Nil | Cons a (List a) 

instance Functor List where 
    fmap _ Nil = Nil 
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where 
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat x) = OneGoat (f x)
    fmap f (MoreGoats gl1 gl2 gl3) = MoreGoats (fmap f gl1) (fmap f gl2) (fmap f gl3)


data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where 
    fmap _ Halt = Halt 
    fmap f (Print str a)  = Print str (f a)
    fmap f (Read strToA) = Read (fmap f strToA) -- or (.) composition instead of fmap

