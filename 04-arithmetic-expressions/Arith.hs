module Arith where

import Data.List (nub)
import Data.Maybe (isNothing)

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

consts :: Term -> [Term]
consts TmTrue  = [TmTrue]
consts TmFalse = [TmFalse]
consts TmZero  = [TmZero]
consts (TmSucc t1) = consts t1
consts (TmPred t1) = consts t1
consts (TmIsZero t1) = consts t1
consts (TmIf t1 t2 t3) = nub (consts t1 ++ consts t2 ++ consts t3)

size :: Term -> Int
size TmTrue  = 1
size TmFalse = 1
size TmZero  = 1
size (TmSucc t1) = size t1 + 1
size (TmPred t1) = size t1 + 1
size (TmIsZero t1) = size t1 + 1
size (TmIf t1 t2 t3) = size t1 + size t2 + size t3 + 1

{- Evaluation -}

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

isNormalForm :: Term -> Bool
isNormalForm = isNothing . eval1

eval :: Term -> Term
eval t = case eval1 t of
           Just t' -> eval t'
           Nothing -> t
