module Lambda where

data Type = Bool
          | Func Type Type
          deriving (Eq)

instance Show Type where
  show Bool = "Bool"
  show (Func t1 t2) = "(" ++ show t1 ++ " → " ++ show t2 ++ ")"

data Term = Tru
          | Fls
          | If Term Term Term
          | Var Int
          | Abs String Type Term
          | App Term Term
          deriving (Eq)

instance Show Term where
  show = showTm []

type Context = [(String, Binding)]

data Binding = NameBind
             | VarBind Type
             deriving (Eq, Show)

showTm :: Context -> Term -> String
showTm _ Tru = "true"
showTm _ Fls = "false"
showTm ctx (If t1 t2 t3) = "(if " ++ show t1 ++ " then " ++ show t2 ++ " else " ++ show t3 ++ ")"
showTm ctx (Var x) = indexToName ctx x
showTm ctx (App t1 t2) = "(" ++ showTm ctx t1 ++ " " ++ showTm ctx t2 ++ ")"
showTm ctx (Abs x _ t1) = "(λ" ++ x' ++ ". " ++ showTm ctx' t1 ++ ")"
  where (ctx', x') = freshName ctx x

indexToName :: Context -> Int -> String
indexToName ctx x
  | x < length ctx = fst (ctx !! x)
  | otherwise = "[" ++ show x ++ "]"

freshName :: Context -> String -> (Context, String)
freshName ctx x = if x `elem` names
                     then freshName ctx (x ++ "'")
                     else ((x, NameBind) : ctx, x)
                  where names = map fst ctx

isVal :: Term -> Bool
isVal (Abs _ _ _) = True
isVal _ = False
