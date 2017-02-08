data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
          deriving (Eq, Show)

{- Syntax -}

isNumericVal :: Term -> Bool
isNumericVal TmZero = True
isNumericVal (TmSucc t1) = isNumericVal t1
isNumericVal _ = False

isVal :: Term -> Bool
isVal TmTrue = True
isVal TmFalse = True
isVal t = isNumericVal t

{- Evaluation -}

eval1 :: Term -> Term

eval1 (TmIf TmTrue t2 t3) = t2           -- E-IfTrue
eval1 (TmIf TmFalse t2 t3) = t3          -- E-IfFalse
eval1 (TmIf t1 t2 t3) = TmIf t1' t2 t3   -- E-If
  where t1' = eval1 t1

eval1 (TmSucc t) = TmSucc t'             -- E-Succ
  where t' = eval1 t

eval1 (TmPred TmZero) = TmZero           -- E-PredZero
eval1 (TmPred (TmSucc nv))
  | isNumericVal nv = nv                 -- E-PredSucc
eval1 (TmPred t) = TmPred t'             -- E-Pred
  where t' = eval1 t

eval1 (TmIsZero TmZero) = TmTrue         -- E-IszeroZero
eval1 (TmIsZero (TmSucc nv))             -- E-IszeroSucc
  | isNumericVal nv = TmFalse
eval1 (TmIsZero t) = TmIsZero t'
  where t' = eval1 t

eval1 t = t                              -- No rule applies

eval :: Term -> Term
eval t
  | t == t' = t
  | otherwise = eval t'
  where t' = eval1 t
