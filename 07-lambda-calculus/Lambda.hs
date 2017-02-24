module Lambda where

data Term = Var Int
          | Abs String Term
          | App Term Term
          deriving (Eq)

tmId = Abs "x" (Var 0)

tmTru = Abs "t" (Abs "f" (Var 1))
tmFls = Abs "t" (Abs "f" (Var 0))
tmTest = Abs "l" (Abs "m" (Abs "n" (App (App (Var 2) (Var 1)) (Var 0))))

tmAnd = Abs "b" (Abs "c" (App (App (Var 1) (Var 0)) tmFls))
tmOr = Abs "b" (Abs "c" (App (App (Var 1) tmTru) (Var 0)))
tmNot = Abs "b" (App (App (Var 0) tmFls) tmTru)

instance Show Term where
  show (Var x) = show x
  show (Abs _ t1) = "(λ. " ++ show t1 ++ ")"
  show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
