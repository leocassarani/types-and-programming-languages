module Funny where

import Arith
import Test.QuickCheck

isNormalForm :: Term -> Bool
isNormalForm = null . eval1

eval :: Term -> [Term]
eval t = case eval1 t of
           [] -> [t]
           ts -> concatMap eval ts

eval1 :: Term -> [Term]
eval1 t = concat [rule t | rule <- evalRules]

evalRules = [ eIfTrue
            , eIfFalse
            , eIf
            , eSucc
            , ePredZero
            , ePredSucc
            , ePred
            , eIsZeroZero
            , eIsZeroSucc
            , eIsZero
            , eFunny1
            , eFunny2
            ]

eIfTrue (TmIf TmTrue t2 t3) = [t2]
eIfTrue _ = []

eIfFalse (TmIf TmFalse t2 t3) = [t3]
eIfFalse _ = []

eIf (TmIf t1 t2 t3) = fmap (\t1' -> TmIf t1' t2 t3) (eval1 t1)
eIf _ = []

eSucc (TmSucc t) = fmap TmSucc (eval1 t)
eSucc _ = []

ePredZero (TmPred TmZero) = [TmZero]
ePredZero _ = []

ePredSucc (TmPred (TmSucc nv))
  | isNumericVal nv = [nv]
ePredSucc _ = []

ePred (TmPred t) = fmap TmPred (eval1 t)
ePred _ = []

eIsZeroZero (TmIsZero TmZero) = [TmTrue]
eIsZeroZero _ = []

eIsZeroSucc (TmIsZero (TmSucc nv))
  | isNumericVal nv = [TmFalse]
eIsZeroSucc _ = []

eIsZero (TmIsZero t) = fmap TmIsZero (eval1 t)
eIsZero _ = []

eFunny1 (TmIf TmTrue t2 t3) = [t3]
eFunny1 _ = []

eFunny2 (TmIf t1 t2 t3) = fmap (\t2' -> TmIf t1 t2' t3) (eval1 t2)
eFunny2 _ = []

-- Theorem 3.5.4: If t -> t' and t -> t'', then t' = t''
prop_determinacyOfOneStepEvaluation t = property $ length (eval1 t) <= 1

-- Theorem 3.5.7: Every value is in normal form
prop_everyValueIsNormalForm t = isVal t ==> isNormalForm t

-- Theorem 3.5.11: If t -*> u and t-*> u', where u and u' are both normal forms, then u = u'
prop_uniquenessOfNormalForms t = property $ length (eval t) <= 1

-- Theorem 3.5.12: For every term t there is some normal form t' such that t -*> t'
prop_terminationOfEvaluation t = property $ any isNormalForm (eval t)

-- Page 39: Each evaluation step reduces the size of the term
prop_evaluationReducesTermSize t = property $ all (\t' -> size t' < size t) (eval1 t)

main = mapM_ (quickCheckWith stdArgs { maxSuccess = 5000 })
  [ prop_determinacyOfOneStepEvaluation
  , prop_everyValueIsNormalForm
  , prop_uniquenessOfNormalForms
  , prop_terminationOfEvaluation
  , prop_evaluationReducesTermSize
  ]
