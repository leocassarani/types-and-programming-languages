module ArithBigStep where

import Arith (Term(..), isNumericVal, isVal, size)

eval :: Term -> Term

eval t@(TmIf t1 t2 t3) =
  case eval t1 of
    TmTrue    -> eval t2 -- B-IfTrue
    TmFalse   -> eval t3 -- B-IfFalse
    otherwise -> t -- No reduction

eval t@(TmSucc t1)
  | isNumericVal t1' = TmSucc t1' -- B-Succ
  | otherwise = t -- No reduction
  where t1' = eval t1

eval t@(TmPred t1) =
  case eval t1 of
    TmZero      -> TmZero  -- B-PredZero
    (TmSucc nv) -> if isNumericVal nv then nv else t -- B-PredSuc
    otherwise   -> t -- No reduction

eval t@(TmIsZero t1) =
  case eval t1 of
    TmZero      -> TmTrue  -- B-IszeroZero
    (TmSucc nv) -> if isNumericVal nv then TmFalse else t -- B-IszeroSucc
    otherwise   -> t -- No reduction

eval t
  | isVal t   = t -- B-Value
  | otherwise = t -- No reduction
