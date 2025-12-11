module ListApl where


data List a =
    Nill
  | Cons a (List a)
  deriving (Eq,Show)

toMyList :: [a] -> List a
toMyList = foldr Cons Nill

fromMyList :: List a -> [a]
fromMyList = fold (:) []

take' :: Int -> List a -> List a
take' 0 l = Nill
take' n (Cons x xs) = Cons x (take' (n-1) xs)

append :: List a -> List a -> List a
append Nill ys = ys
append (Cons x xs) ys = x `Cons` append xs ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nill = b
fold f b (h `Cons` t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nill

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' f Nill ys = Nill
zipWith' f xs Nill = Nill
zipWith' f (Cons x xs) (Cons y ys) = f x y `Cons` zipWith' f xs ys

instance Functor List where
    fmap f Nill = Nill
    fmap f (Cons first rest) = f first `Cons` fmap f rest


instance Applicative List where
    pure x = Cons x Nill
    (<*>) Nill l = Nill
    (<*>) fs Nill = Nill
    (<*>) fs lx = flatMap (\f -> fmap f lx) fs


functions = toMyList [(+1), (*2)]
values = toMyList [1,2]

test = (functions <*> values) == toMyList [2,3,2,4]

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' (Cons x Nill)

  (ZipList' fs) <*> (ZipList' vs) = ZipList' $ zipWith' ($) fs vs

z = ZipList' $ toMyList [(+9), (*2), (+8)]
z' = ZipList' $ toMyList [1,2,3]

test1 = (z <*> z') == ZipList' (toMyList [10, 4, 11])


data Validation e a = 
    Failure e
  | Success a 
  deriving (Eq, Show)

data Error = 
    DividedByZero 
  | StackOverflow 
  | MoogleChewedWires 
  deriving (Eq, Show)
  

instance Functor (Validation e) where 
  fmap f (Success x) = Success $ f x 
  fmap f (Failure e) = Failure e 

instance Monoid e => 
         Applicative (Validation e) where 
  pure = Success 

  (Failure e1) <*> (Failure e2) = Failure $ e1 <> e2 
  (Failure e1) <*> v = Failure e1 
  (Success f) <*> v = fmap f v  

success :: Validation [Error] Integer
success = Success (+1) <*> Success 1 

failure = Success (+1) <*> Failure [StackOverflow]

failure' = Failure [StackOverflow] <*> Success (+1)


failures = 
      Failure [MoogleChewedWires] 
  <*> Failure [StackOverflow]


