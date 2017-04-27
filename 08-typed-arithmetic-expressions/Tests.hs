module Tests where

import Arith
import Eval
import TypeCheck

import Control.Monad (liftM, liftM3)
import Data.Maybe (isJust)
import Test.QuickCheck

main = mapM_ (quickCheckWith stdArgs { maxSuccess = 50000 })
  [ prop_progress
  , prop_preservation
  ]

-- Theorem 8.3.2: Suppose t is a well-typed term. Then either t is a value
-- or else there is some t' with t → t'.
prop_progress t = isWellTyped t ==> isVal t || canEval t

-- Theorem 8.3.3: If t : T and t → t', then t' : T'.
prop_preservation t = isWellTyped t && canEval t ==> maybe False isWellTyped (eval1 t)

isWellTyped :: Term -> Bool
isWellTyped = isJust . typeOf

canEval :: Term -> Bool
canEval = isJust . eval1

instance Arbitrary Term where
  arbitrary = oneof [genNat, genBool]

  shrink (TmIf t1 t2 t3) =
    [t1, t2, t3] ++
    [TmIf t1' t2 t3 | t1' <- shrink t1] ++
    [TmIf t1 t2' t3 | t2' <- shrink t2] ++
    [TmIf t1 t2 t3' | t3' <- shrink t3]
  shrink (TmSucc t1) = t1 : [TmSucc t1' | t1' <- shrink t1]
  shrink (TmPred t1) = t1 : [TmPred t1' | t1' <- shrink t1]
  shrink (TmIsZero t1) = t1 : [TmIsZero t1' | t1' <- shrink t1]
  shrink _ = []

genNat :: Gen Term
genNat = oneof [ return TmZero
               , return (TmSucc TmZero)
               , return (TmPred TmZero)
               , liftM TmSucc genNat
               , liftM TmPred genNat
               , liftM3 TmIf genBool genNat genNat
               ]

genBool :: Gen Term
genBool = oneof [ return TmTrue
                , return TmFalse
                , liftM TmIsZero genNat
                , liftM3 TmIf genBool genBool genBool
                ]
