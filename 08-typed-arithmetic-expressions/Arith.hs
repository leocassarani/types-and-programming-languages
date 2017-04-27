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

isVal :: Term -> Bool
isVal TmTrue = True
isVal TmFalse = True
isVal t = isNumericVal t

isNumericVal :: Term -> Bool
isNumericVal TmZero = True
isNumericVal (TmSucc t1) = isNumericVal t1
isNumericVal _ = False

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
