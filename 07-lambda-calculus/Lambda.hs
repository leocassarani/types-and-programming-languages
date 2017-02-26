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
  show = showTm []

type Context = [String]

showTm :: Context -> Term -> String
showTm ctx (Var x) = indexToName ctx x
showTm ctx (App t1 t2) = "(" ++ showTm ctx t1 ++ " " ++ showTm ctx t2 ++ ")"
showTm ctx (Abs x t1) = "(λ" ++ x' ++ ". " ++ showTm ctx' t1 ++ ")"
  where (ctx', x') = freshName ctx x

indexToName :: Context -> Int -> String
indexToName ctx x
  | x < length ctx = ctx !! x
  | otherwise = "[" ++ show x ++ "]"

freshName :: Context -> String -> (Context, String)
freshName ctx x
  | x `elem` ctx = freshName ctx (x ++ "'")
  | otherwise = (x : ctx, x)

termShift :: Int -> Term -> Term
termShift d t = termShift' 0 t
  where
    termShift' c t@(Var x)
      | x >= c = Var (x + d)
      | otherwise = t
    termShift' c (Abs x t1) = Abs x (termShift' (c + 1) t1)
    termShift' c (App t1 t2) = App (termShift' c t1) (termShift' c t2)

termSub :: Int -> Term -> Term -> Term
termSub j s t = termSub' 0 t
  where
    termSub' c t@(Var x)
      | x == j + c = termShift c s
      | otherwise = t
    termSub' c (Abs x t1) = Abs x (termSub' (c + 1) t1)
    termSub' c (App t1 t2) = App (termSub' c t1) (termSub' c t2)
