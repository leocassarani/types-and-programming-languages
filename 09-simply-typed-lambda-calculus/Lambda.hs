module Lambda where

import Data.List (intercalate)

data Type = Bool
          | UnitType
          | Func Type Type
          | TupleType [Type]
          | RecordType [(String, Type)]
          | Variant [(String, Type)]
          deriving (Eq)

instance Show Type where
  show Bool = "Bool"
  show UnitType = "Unit"
  show (Func t1 t2) = "(" ++ show t1 ++ " → " ++ show t2 ++ ")"
  show (TupleType types) = "{" ++ intercalate ", " (map show types) ++ "}"
  show (RecordType entries) = "{" ++ intercalate ", " (map showEntry entries) ++ "}"
    where showEntry (l, t) = l ++ "=" ++ show t
  show (Variant branches) =  "<" ++ intercalate ", " (map showBranch branches) ++ ">"
    where showBranch (l, t) = l ++ ":" ++ show t

data Term = Tru
          | Fls
          | If Term Term Term
          | Var Int
          | Abs String Type Term
          | App Term Term
          | Unit
          | As Term Type
          | Let String Term Term
          | Tuple [Term]
          | TupleProject Term Int
          | Record [(String, Term)]
          | RecordProject Term String
          | Tag String Term Type
          | Case Term [(String, String, Term)]
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
showTm ctx (Var x) = indexToName ctx x
showTm ctx (App t1 t2) = "(" ++ showTm ctx t1 ++ " " ++ showTm ctx t2 ++ ")"
showTm ctx (Abs x typ t1) = "(λ" ++ x' ++ ":" ++ show typ ++ ". " ++ showTm ctx' t1 ++ ")"
  where (ctx', x') = freshName ctx x
showTm ctx (As t1 typ) = showTm ctx t1 ++ " as " ++ show typ
showTm ctx (Let x t1 t2) = "let " ++ x ++ " = " ++ showTm ctx t1 ++ " in " ++ showTm ctx t2
showTm ctx (Tuple terms) = "{" ++ intercalate ", " termStrings ++ "}"
  where termStrings = map (showTm ctx) terms
showTm ctx (TupleProject term idx) = showTm ctx term ++ "." ++ show idx
showTm ctx (Record entries) = "{" ++ intercalate ", " (map showEntry entries) ++ "}"
  where showEntry (l, t) = l ++ "=" ++ showTm ctx t
showTm ctx (RecordProject term label) = showTm ctx term ++ "." ++ label
showTm ctx (Tag label term typ) = "<" ++ label ++ " = " ++ showTm ctx term ++ "> as " ++ show typ
showTm ctx (Case variant branches) = "case (" ++ showTm ctx variant ++ ") of " ++ intercalate " | " (map showBranch branches)
  where showBranch (label, var, term) = "<" ++ label ++ " = " ++ var ++ ">" ++ " ⇒ " ++ showTm ctx term

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
isVal (Tuple terms) = all isVal terms
isVal (Record entries) = all (isVal . snd) entries
isVal _ = False
