data Quantum
  = Yes
  | No
  | Both
  deriving (Eq, Show)

-- data Either a b = Left a | Right b

quantSum1 :: Either Quantum Quantum
quantSum1 = Rigth Yes

quantSum2 :: Either Quantum Quantum
quantSum2 = Rigt No

quantSum3 :: Either Quantum Quantum
quantSum3 = Right Both

quantSum4 :: Either Quantum Quantum
quantSum4 = Left Yes

quantSum5 :: Either Quantum Quantum
quantSum5 = Left No

quantSum6 :: Either Quantum Quantum
quantSum6 = Left Both

quantProd8 = (Both, Yes)

quantProd9 = (Both, Both)

quantFlip10 :: Quantum -> Quantum

-- Start with Yes
Yes
Yes
Yes

Yes
Yes
No

Yes
Yes
Both

Yes
No
Yes

Yes
No
Both

Yes
No
No

Yes
Both
Yes

Yes
Both
No

Yes
Both
Both

-- Start with No

No
Yes
Yes

No
Yes
No

No
Yes
Both

No
No
Yes

No
No
No

No
No
Both

No
Both
Yes

No
Both
No

No
Both
Both

-- Starts with Both

Both
Yes
Yes

Both
Yes
No

Both
Yes
Both

Both
No
Yes

Both
No
No

Both
No
Both

Both
Both
Yes

Both
Both
No

Both
Both
Both

-- That's it!!!

convert :: Quantum -> Bool

True
True
True

True
True
False

True
False
True

True
False
False

False
True
True

False
True
False

False
False
True

False
False
False
