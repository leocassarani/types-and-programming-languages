module Tests where

import Arith
import Eval
import TypeCheck

import Data.Maybe (isJust)
import Test.QuickCheck

main = mapM_ (quickCheckWith stdArgs { maxDiscardRatio = 100, maxSuccess = 50000 })
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
