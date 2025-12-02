import Data.Int

data BigSmall
  = Big Bool
  | Small Bool
  deriving (Eq, Show)

data NumberOrBool
  = Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)

data QuantumBool
  = QuantumTrue
  | QuantumFalse
  | QuantumBoth
  deriving (Eq, Show)

data TwoQs
  = MkTwoQs
      QuantumBool
      QuantumBool
  deriving (Eq, Show)

data Personf = MkPerson String Int deriving (Eq, Show)

data Person
  = Person {name :: String, age :: Int}
  deriving (Eq, Show)

-- data Fiction
--   = MkFiction
--   deriving (Show)

-- data Nonfiction
--   = MkNonfiction
--   deriving (Show)

type AuthorName = String

data Autor
  = Fiction AuthorName
  | NonFiction AuthorName
  deriving (Eq, Show)

type Gardener = String

data Garden
  = Daisy Gardener
  | Rose Gardener
  | Lilac Gardener
  deriving (Show)