module Tests where

import Lambda
import Eval
import Generator
import TypeCheck

import Data.Maybe (fromJust, isJust)
import Test.QuickCheck

main = quickCheckWith stdArgs { maxSuccess = 50000 } prop_safety

prop_safety t = prop_progress t .&&. prop_preservation t

prop_progress t = isWellTyped t ==> isVal t || canEval t

prop_preservation t = isWellTyped t && canEval t ==> justTypeOf t == justTypeOf (justEval t)
  where justTypeOf = fromJust . (typeOf [])
        justEval = fromJust . eval1

isWellTyped :: Term -> Bool
isWellTyped = isJust . (typeOf [])

canEval :: Term -> Bool
canEval = isJust . eval1
