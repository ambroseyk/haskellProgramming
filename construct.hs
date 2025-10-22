data GuessWhat = Chickenbutt deriving (Eq, Show)

data Id a = MkId a deriving (Eq, Show)

idInt :: Id Integer
idInt = MkId 10

idIdentity :: Id (a -> a)
idIdentity = MkId (\x -> x)

data Product a b = Product a b deriving (Eq, Show)

data Sum a b = First a | Second b deriving (Eq, Show)

type Awesome = Bool

type Name = String

person :: Product Name Awesome
person = Product "simon" True

type Twitter = String

type AskFm = String

data SocialNetwork
  = Twitter
  | AskFm
  deriving (Eq, Show)

twitter :: Sum Twitter AskFm
twitter = First "Twitter"

askfm :: Sum Twitter AskFm
askfm = First "AskFm"

data RecordProduct a b
  = RecordProduct
  { pfirst :: a,
    psecond :: b
  }
  deriving (Eq, Show)

myRecord :: RecordProduct Integer Float
-- myRecord = RecordProduct 42 0.000001
myRecord = RecordProduct {pfirst = 42, psecond = 0.0001}

data OperatingSystem
  = GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgrammingLanguage
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer
  = Programmer {os :: OperatingSystem, lang :: ProgrammingLanguage}
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux,
    OpenBSDPlusNevermindJustBSDStill,
    Mac,
    Windows
  ]

allProgrammers :: [Programmer]
allProgrammers = [Programmer os lang | os <- allOperatingSystems, lang <- allLanguages]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

nineToFive :: Programmer
nineToFive = Programmer {os = Mac, lang = Haskell}

feelingWizardly :: Programmer
feelingWizardly = Programmer {lang = Agda, os = GnuPlusLinux}

data ThereYet
  = There Integer Float String Bool
  deriving (Eq, Show)

nope :: Float -> String -> Bool -> ThereYet
nope = There 10

notYet :: String -> Bool -> ThereYet
notYet = nope 1.2453

notQuite :: Bool -> ThereYet
notQuite = notYet "woohooo"

yusss :: ThereYet
yusss = notQuite True

newtype NumCow
  = NumCow Int
  deriving (Eq, Show)

newtype NumPig
  = NumPig Int
  deriving (Eq, Show)

data Farmhouse
  = Farmhouse NumCow NumPig
  deriving (Show, Eq)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep = NumSheep Int deriving (Eq, Show)

data BigFarmhouse
  = BigFarmhouse NumCow NumPig NumSheep
  deriving (Eq, Show)

type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

type Age = Int

type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo = CowInfo Name Age
  deriving (Eq, Show)

data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)

data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)

data Animal
  = Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)

type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

bess = First (CowInfo "Bess" 4) :: Animal'

elmer' = Second (Second (SheepInfo "Elmer" 5 5)) :: Animal'

-- elmo' = First (Second (SheepInfo "Elmo" 5 5)) :: Animal'

bob :: Sum (Sum a SheepInfo) b
bob = First (Second (SheepInfo "Baaa" 5 5))
