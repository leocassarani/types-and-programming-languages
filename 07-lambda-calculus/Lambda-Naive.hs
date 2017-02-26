module Lambda where

import Data.List (delete, nub)

data Term = Var String
          | Abs String Term
          | App Term Term
          deriving (Eq)

instance Show Term where
  show (Var x) = x
  show (Abs x t) = "(λ" ++ x ++ ". " ++ show t ++ ")"
  show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

idLT = Abs "x" (Var "x")

truLT = Abs "t" (Abs "f" (Var "t"))
flsLT = Abs "t" (Abs "f" (Var "f"))

testLT = Abs "l" (Abs "m" (Abs "n" (App (App (Var "l") (Var "m")) (Var "n"))))

andLT = Abs "b" (Abs "c" (App (App (Var "b") (Var "c")) flsLT))
orLT  = Abs "b" (Abs "c" (App (App (Var "b") truLT) (Var "c")))
notLT = Abs "b" (App (App (Var "b") flsLT) truLT)

pairLT = Abs "f" (Abs "s" (Abs "b" (App (App (Var "b") (Var "f")) (Var "s"))))
fstLT = Abs "p" (App (Var "p") truLT)
sndLT = Abs "p" (App (Var "p") flsLT)

isCombinator :: Term -> Bool
isCombinator = null . free

size :: Term -> Int
size (Var x) = 1
size (Abs x t1) = size t1 + 1
size (App t1 t2) = size t1 + size t2 + 1

free :: Term -> [Term]
free t@(Var x) = [t]
free (Abs x t1) = delete (Var x) (free t1)
free (App t1 t2) = nub (free t1 ++ free t2)
