module Lambda where

data Type = Bool
          | Func Type Type
          | UnitType
          deriving (Eq)

instance Show Type where
  show Bool = "Bool"
  show UnitType = "Unit"
  show (Func t1 t2) = "(" ++ show t1 ++ " → " ++ show t2 ++ ")"

data Term = Tru
          | Fls
          | If Term Term Term
          | Var Int
          | Abs String Type Term
          | App Term Term
          | Unit
          | As Term Type
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
showTm _ Unit = "unit"
showTm ctx (If t1 t2 t3) = "(if " ++ showTm ctx t1 ++ " then " ++ showTm ctx t2 ++ " else " ++ showTm ctx t3 ++ ")"
showTm ctx (As t1 typ) = showTm ctx t1 ++ " as " ++ show typ
showTm ctx (Var x) = indexToName ctx x
showTm ctx (App t1 t2) = "(" ++ showTm ctx t1 ++ " " ++ showTm ctx t2 ++ ")"
showTm ctx (Abs x typ t1) = "(λ" ++ x' ++ ":" ++ show typ ++ ". " ++ showTm ctx' t1 ++ ")"
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
isVal Tru = True
isVal Fls = True
isVal Unit = True
isVal (Abs _ _ _) = True
isVal _ = False
