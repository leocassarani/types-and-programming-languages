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

eval1 :: Term -> Maybe Term

eval1 (TmIf TmTrue t2 t3) = Just t2      -- E-IfTrue
eval1 (TmIf TmFalse t2 t3) = Just t3     -- E-IfFalse
eval1 (TmIf t1 t2 t3)                    -- E-If
  = case eval1 t1 of
      Just t1' -> Just (TmIf t1' t2 t3)
      Nothing  -> Nothing

eval1 (TmSucc t)                         -- E-Succ
  = case eval1 t of
      Just t' -> Just (TmSucc t')
      Nothing -> Nothing

eval1 (TmPred TmZero) = Just TmZero      -- E-PredZero
eval1 (TmPred (TmSucc nv))
  | isNumericVal nv = Just nv            -- E-PredSucc
eval1 (TmPred t)                         -- E-Pred
  = case eval1 t of
      Just t' -> Just (TmPred t')
      Nothing -> Nothing

eval1 (TmIsZero TmZero) = Just TmTrue    -- E-IszeroZero
eval1 (TmIsZero (TmSucc nv))             -- E-IszeroSucc
  | isNumericVal nv = Just TmFalse
eval1 (TmIsZero t)
  = case eval1 t of
      Just t' -> Just (TmIsZero t')
      Nothing -> Nothing

eval1 _ = Nothing                        -- No rule applies

eval :: Term -> Term
eval t = case eval1 t of
           Just t' -> eval t'
           Nothing -> t
