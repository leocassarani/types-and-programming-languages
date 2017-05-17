module Tests where

import Lambda
import Eval
import TypeCheck

import Data.Maybe (isJust)
import Test.QuickCheck

main = mapM_ (quickCheckWith stdArgs { maxSuccess = 50000 })
  [ prop_progress
  --, prop_preservation
  ]

prop_progress t = isWellTyped t ==> isVal t || canEval t

isWellTyped :: Term -> Bool
isWellTyped = isJust . (typeOf [])

canEval :: Term -> Bool
canEval = isJust . eval1

instance Arbitrary Term where
  arbitrary = oneof [ return TmVar

