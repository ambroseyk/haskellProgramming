module EqCaseGuard where

data PersonInvalid
    = NameEmpty
    | AgeTooLow

instance Show PersonInvalid where
    show :: PersonInvalid -> String
    show = blah
    -- show = toString

instance Eq PersonInvalid where
    (==) :: PersonInvalid -> PersonInvalid -> Bool
    (==) NameEmpty NameEmpty = True
    (==) NameEmpty AgeTooLow = False
    (==) AgeTooLow NameEmpty = True
    (==) AgeTooLow AgeTooLow = True 

toString :: PersonInvalid -> String
toString NameEmpty = "NameEmpty"
toString AgeTooLow = "AgeTooLow"

blah :: PersonInvalid -> String
blah pi
    | pi == NameEmpty = "NameEmpty"
    | pi == AgeTooLow = "AgeTooLow"
    | otherwise = "?"



