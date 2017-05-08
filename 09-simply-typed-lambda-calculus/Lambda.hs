module Lambda where

import Data.List hiding ((!!))

data Term = Tru
          | Fls
          | If Term Term Term
          | Var Int
          | Abs String Type Term
          | App Term Term
          deriving (Eq)

data Type = Bool
          | Func Type Type
          deriving (Eq, Show)

instance Show Term where
  show = showTm []

type Context = [(String, Binding)]

data Binding = NameBind
             | VarBind Type
             deriving (Eq, Show)

showTm :: Context -> Term -> String
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

termShift :: Int -> Term -> Term
termShift d = termShift' 0
  where
    termShift' c t@(Var x)
      | x >= c = Var (x + d)
      | otherwise = t
    termShift' c (Abs x typ t1) = Abs x typ (termShift' (c + 1) t1)
    termShift' c (App t1 t2) = App (termShift' c t1) (termShift' c t2)

termSub :: Int -> Term -> Term -> Term
termSub j s = termSub' 0
  where
    termSub' c t@(Var x)
      | x == j + c = termShift c s
      | otherwise = t
    termSub' c (Abs x typ t1) = Abs x typ (termSub' (c + 1) t1)
    termSub' c (App t1 t2) = App (termSub' c t1) (termSub' c t2)

termSubTop :: Term -> Term -> Term
termSubTop s t = termShift (-1) (termSub 0 (termShift 1 s) t)

isVal :: Term -> Bool
isVal (Abs _ _ _) = True
isVal _ = False

eval1 :: Term -> Maybe Term
eval1 (App (Abs x _ t12) v2)
  | isVal v2 = Just (termSubTop v2 t12) -- E-AppAbs
eval1 (App t1 t2)
  | isVal t1  = fmap (\t2' -> App t1 t2') (eval1 t2) -- E-App2
  | otherwise = fmap (\t1' -> App t1' t2) (eval1 t1) -- E-App1
eval1 _ = Nothing

eval :: Term -> Term
eval t = maybe t eval (eval1 t)

typeOf :: Context -> Term -> Maybe Type

typeOf _ Tru = Just Bool      -- T-True

typeOf _ Fls = Just Bool      -- T-False

typeOf ctx (If t1 t2 t3) = do -- T-If
  typ1 <- typeOf ctx t1
  typ2 <- typeOf ctx t2
  typ3 <- typeOf ctx t3
  if typ2 == typ3
     then Just typ2
     else Nothing

typeOf ctx (Var x) = typeFromContext ctx x -- T-Var

typeOf ctx (Abs x typ1 t1) = -- T-Abs
  typeOf ctx' t1 >>= Just . Func typ1
    where ctx' = (x, VarBind typ1) : ctx

typeOf ctx (App t1 t2) = do -- T-App
  typ1 <- typeOf ctx t1
  typ2 <- typeOf ctx t2
  case typ1 of
    Func typ11 typ12 -> if typ11 == typ2
                           then Just typ12
                           else Nothing
    otherwise -> Nothing

typeFromContext :: Context -> Int -> Maybe Type
typeFromContext ctx x = do
  binding <- snd <$> ctx `index` x
  case binding of
    VarBind typ -> Just typ
    otherwise -> Nothing

index :: [a] -> Int -> Maybe a
index [] _ = Nothing
index (x:_) 0 = Just x
index (x:xs) i = index xs (i - 1)
