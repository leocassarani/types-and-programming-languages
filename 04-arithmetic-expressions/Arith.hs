module Arith where

import Control.Monad (liftM, liftM3)
import Data.List (nub)
import Test.QuickCheck

data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
          deriving (Eq, Show)

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

instance Arbitrary Term where
  arbitrary = oneof [ return TmTrue
                    , return TmFalse
                    , return TmZero
                    , liftM TmSucc arbitrary
                    , liftM TmPred arbitrary
                    , liftM TmIsZero arbitrary
                    , liftM3 TmIf arbitrary arbitrary arbitrary
                    ]

  shrink (TmIf t1 t2 t3) =
    [t1, t2, t3] ++
    [TmIf t1' t2 t3 | t1' <- shrink t1] ++
    [TmIf t1 t2' t3 | t2' <- shrink t2] ++
    [TmIf t1 t2 t3' | t3' <- shrink t3]
  shrink (TmSucc t1) = t1 : [TmSucc t1' | t1' <- shrink t1]
  shrink (TmPred t1) = t1 : [TmPred t1' | t1' <- shrink t1]
  shrink (TmIsZero t1) = t1 : [TmIsZero t1' | t1' <- shrink t1]
  shrink _ = []
