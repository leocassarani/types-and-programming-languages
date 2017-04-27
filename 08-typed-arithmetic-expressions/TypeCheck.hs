module TypeCheck where

import Arith

data Type = Bool | Nat
  deriving (Eq, Show)

typeOf :: Term -> Maybe Type

typeOf TmTrue = Just Bool    -- T-True

typeOf TmFalse = Just Bool   -- T-False

typeOf (TmIf t1 t2 t3)       -- T-If
  = case (typeOf t1, typeOf t2, typeOf t3) of
      (Just Bool, Just typ2, Just typ3)
        | typ2 == typ3 -> Just typ2
      otherwise -> Nothing

typeOf TmZero = Just Nat     -- T-Zero

typeOf (TmSucc t1)           -- T-Succ
  = case typeOf t1 of
      Just Nat -> Just Nat
      otherwise -> Nothing

typeOf (TmPred t1)           -- T-Pred
  = case typeOf t1 of
      Just Nat -> Just Nat
      otherwise -> Nothing

typeOf (TmIsZero t1)         -- T-IsZero
  = case typeOf t1 of
      Just Nat -> Just Bool
      otherwise -> Nothing
