module Eval where

import Arith

eval :: Term -> Term
eval t = case eval1 t of
           Just t' -> eval t'
           Nothing -> t

eval1 :: Term -> Maybe Term

eval1 (TmIf TmTrue t2 t3) = Just t2            -- E-IfTrue
eval1 (TmIf TmFalse t2 t3) = Just t3           -- E-IfFalse
eval1 (TmIf t1 t2 t3)                          -- E-If
  = fmap (\t1' -> TmIf t1' t2 t3) (eval1 t1)

eval1 (TmSucc t) = fmap TmSucc (eval1 t)       -- E-Succ

eval1 (TmPred TmZero) = Just TmZero            -- E-PredZero
eval1 (TmPred (TmSucc nv))
  | isNumericVal nv = Just nv                  -- E-PredSucc
eval1 (TmPred t) = fmap TmPred (eval1 t)       -- E-Pred

eval1 (TmIsZero TmZero) = Just TmTrue          -- E-IszeroZero
eval1 (TmIsZero (TmSucc nv))                   -- E-IszeroSucc
  | isNumericVal nv = Just TmFalse
eval1 (TmIsZero t) = fmap TmIsZero (eval1 t)   -- E-IsZero

eval1 _ = Nothing                              -- No rule applies
