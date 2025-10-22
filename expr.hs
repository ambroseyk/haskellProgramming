data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add exp1 exp2) = eval exp1 + eval exp2

myExp :: Expr
myExp = (Add (Lit 1) (Lit 9001))

testEval :: IO ()
testEval = if eval myExp == 9002 then print "testEval: Sucess" else print "testEval: Failed"

printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add exp1 exp2) = printExpr exp1 ++ " + " ++ printExpr exp2

a1 = Add (Lit 1) (Lit 9001)
a2 = Add a1 (Lit 2001)
a3 = Add (Lit 1) a2 