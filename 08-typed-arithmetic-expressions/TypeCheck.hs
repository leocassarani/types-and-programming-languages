module TypeCheck where

import Arith
import Control.Monad (guard)

data Type = Bool | Nat
  deriving (Eq, Show)

typeOf :: Term -> Maybe Type

typeOf TmTrue = return Bool -- T-True
typeOf TmFalse = return Bool -- T-False

typeOf (TmIf t1 t2 t3) = do -- T-If
  Bool <- typeOf t1
  typ2 <- typeOf t2
  typ3 <- typeOf t3
  guard (typ2 == typ3)
  return typ2

typeOf TmZero = return Nat -- T-Zero

typeOf (TmSucc t1) = do -- T-Succ
  Nat <- typeOf t1
  return Nat

typeOf (TmPred t1) = do -- T-Pred
  Nat <- typeOf t1
  return Nat

typeOf (TmIsZero t1) = do -- T-IsZero
  Nat <- typeOf t1
  return Bool
