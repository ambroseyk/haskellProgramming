newtype Name = Name String deriving (Show)

newtype Acres
  = Acres Int
  deriving (Show)

data FarmerType
  = DairyFarmer
  | WheatFarmer
  | SoybeanFarmer
  deriving (Show)

data Farmer
  = Farmer Name Acres FarmerType
  deriving (Show)

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

data FarmerRec
  = FarmerRec
  { name :: Name,
    acres :: Acres,
    farmerType :: FarmerType
  }
  deriving (Show)

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec (FarmerRec _ _ DairyFarmer) = True
isDairyFarmerRec _ = False

-- isDairyFarmerRec fr = case farmerType fr of
--   DairyFarmer -> True
--   _ -> False

data Automobile
  = Null
  | Automobile Car

data Car
  = Car
  { make :: String,
    model :: String,
    year :: Integer
  }
  deriving (Eq, Show)
