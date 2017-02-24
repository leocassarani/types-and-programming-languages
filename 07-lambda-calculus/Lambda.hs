module Lambda where

data Term = Var String
          | Abs String Term
          | App Term Term
          deriving (Eq)

instance Show Term where
  show (Var x) = x
  show (Abs x t) = "λ" ++ x ++ ". " ++ show t
  show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

idLT = Abs "x" (Var "x")

tru = Abs "t" (Abs "f" (Var "t"))
fls = Abs "t" (Abs "f" (Var "f"))

testLT = Abs "l" (Abs "m" (Abs "n" (App (App (Var "l") (Var "m")) (Var "n"))))

andLT = Abs "b" (Abs "c" (App (App (Var "b") (Var "c")) (Var "fls")))
orLT  = Abs "b" (Abs "c" (App (App (Var "b") (Var "tru")) (Var "c")))
notLT = Abs "b" (App (App (Var "b") (Var "fls")) (Var "tru"))

pairLT = Abs "f" (Abs "s" (Abs "b" (App (App (Var "b") (Var "f")) (Var "s"))))
fstLT = Abs "p" (App (Var "p") tru)
sndLT = Abs "p" (App (Var "p") fls)

eval :: Term -> Term
eval t
  | t == t' = t
  | otherwise = eval t'
  where t' = reduce t

reduce :: Term -> Term
reduce (App (Abs x t12) t2) = bind x t2 t12
reduce t = t

bind :: String -> Term -> Term -> Term
bind x t (Var y)
  | x == y = t
  | otherwise = Var y
bind x t1 (Abs y t2) = Abs y (bind x t1 t2)
bind x t1 (App t2 t3) = App (bind x t1 t2) (bind x t1 t3)
