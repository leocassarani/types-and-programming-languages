module Arith where

import Control.Monad (liftM, liftM3)

data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
          deriving (Eq, Show)

isVal :: Term -> Bool
isVal TmTrue = True
isVal TmFalse = True
isVal t = isNumericVal t

isNumericVal :: Term -> Bool
isNumericVal TmZero = True
isNumericVal (TmSucc t1) = isNumericVal t1
isNumericVal _ = False
