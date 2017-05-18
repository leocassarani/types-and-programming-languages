module Eval where

import Lambda

termShift :: Int -> Term -> Term
termShift d = termShift' 0
  where
    termShift' c t@(Var x)
      | x >= c = Var (x + d)
      | otherwise = t
    termShift' c (Abs x typ t1) = Abs x typ (termShift' (c + 1) t1)
    termShift' c (App t1 t2) = App (termShift' c t1) (termShift' c t2)
    termShift' c (If t1 t2 t3) = If (termShift' c t1) (termShift' c t2) (termShift' c t3)
    termShift' _ t = t

termSub :: Int -> Term -> Term -> Term
termSub j s = termSub' 0
  where
    termSub' c t@(Var x)
      | x == j + c = termShift c s
      | otherwise = t
    termSub' c (Abs x typ t1) = Abs x typ (termSub' (c + 1) t1)
    termSub' c (App t1 t2) = App (termSub' c t1) (termSub' c t2)
    termSub' c (If t1 t2 t3) = If (termSub' c t1) (termSub' c t2) (termSub' c t3)
    termSub' _ t = t

termSubTop :: Term -> Term -> Term
termSubTop s t = termShift (-1) (termSub 0 (termShift 1 s) t)

eval1 :: Term -> Maybe Term
eval1 (If Tru t _) = Just t
eval1 (If Fls _ t) = Just t
eval1 (If t1 t2 t3) = fmap (\t1' -> If t1' t2 t3) (eval1 t1)
eval1 (App (Abs x _ t12) v2)
  | isVal v2 = Just (termSubTop v2 t12) -- E-AppAbs
eval1 (App t1 t2)
  | isVal t1  = fmap (\t2' -> App t1 t2') (eval1 t2) -- E-App2
  | otherwise = fmap (\t1' -> App t1' t2) (eval1 t1) -- E-App1
eval1 _ = Nothing

eval :: Term -> Term
eval t = maybe t eval (eval1 t)
