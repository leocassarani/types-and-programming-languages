module SmallStep where

import Arith
import Data.Maybe (isNothing)
import Test.QuickCheck

isNormalForm :: Term -> Bool
isNormalForm = isNothing . eval1

eval :: Term -> Term
eval t = case eval1 t of
           Just t' -> eval t'
           Nothing -> t

eval1 :: Term -> Maybe Term

eval1 (TmIf TmTrue t2 t3) = Just t2            -- E-IfTrue
eval1 (TmIf TmFalse t2 t3) = Just t3           -- E-IfFalse
eval1 (TmIf t1 t2 t3)                          -- E-If
  = fmap (\t1' -> TmIf t1' t2 t3) (eval1 t1)

eval1 (TmSucc t) = fmap TmSucc (eval1 t)       -- E-Succ

eval1 (TmPred TmZero) = Just TmZero            -- E-PredZero
eval1 (TmPred (TmSucc nv))
  | isNumericVal nv = Just nv                  -- E-PredSucc
eval1 (TmPred t) = fmap TmPred (eval1 t)       -- E-Pred

eval1 (TmIsZero TmZero) = Just TmTrue          -- E-IszeroZero
eval1 (TmIsZero (TmSucc nv))                   -- E-IszeroSucc
  | isNumericVal nv = Just TmFalse
eval1 (TmIsZero t) = fmap TmIsZero (eval1 t)   -- E-IsZero

eval1 _ = Nothing                              -- No rule applies

-- Lemma 3.3.3: The number of distinct constants in a term t is no greater
-- than the size of t (i.e., |Consts(t)| ≤ size(t))
prop_constsNoGreaterThanSize t = property $ length (consts t) <= size t

-- Theorem 3.5.7: Every value is in normal form
prop_everyValueIsNormalForm t = isVal t ==> isNormalForm t

-- Theorem 3.5.12: For every term t there is some normal form t' such that t -*> t'
prop_terminationOfEvaluation t = property $ isNormalForm (eval t)

-- Page 39: Each evaluation step reduces the size of the term
prop_evaluationReducesTermSize t = property $ maybeSize (eval1 t) < size t
  where maybeSize = maybe 0 size

main = mapM_ (quickCheckWith stdArgs { maxSuccess = 5000 })
  [ prop_constsNoGreaterThanSize
  , prop_everyValueIsNormalForm
  , prop_terminationOfEvaluation
  , prop_evaluationReducesTermSize
  ]
