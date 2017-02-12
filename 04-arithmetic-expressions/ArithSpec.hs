import Arith
import Control.Monad (liftM, liftM3)
import Data.List (nub)
import Data.Maybe (isNothing)
import Test.QuickCheck

main = mapM_ (quickCheckWith stdArgs { maxSuccess = 5000 })
  [ prop_constsNoGreaterThanSize
  , prop_everyValueIsNormalForm
  , prop_everyBoolNormalFormIsValue
  , prop_terminationOfEvaluation
  , prop_evaluationReducesTermSize
  ]

genBool = oneof [return TmTrue,
                 return TmFalse,
                 liftM3 TmIf genBool genBool genBool]

genTerm = oneof [return TmTrue,
                 return TmFalse,
                 return TmZero,
                 liftM TmSucc genTerm,
                 liftM TmPred genTerm,
                 liftM TmIsZero genTerm,
                 liftM3 TmIf genTerm genTerm genTerm]

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

isNormalForm :: Term -> Bool
isNormalForm = isNothing . eval1

-- Lemma 3.3.3: The number of distinct constants in a term t is no greater
-- than the size of t (i.e., |Consts(t)| ≤ size(t))
prop_constsNoGreaterThanSize = forAll genTerm $ \t -> length (consts t) <= size t

-- Theorem 3.5.7: Every value is in normal form
prop_everyValueIsNormalForm = forAll genTerm $ \t -> isVal t ==> isNormalForm t

-- Theorem 3.5.8: If t is in normal form, then t is a value (for Boolean values)
prop_everyBoolNormalFormIsValue = forAll genBool $ \t -> isNormalForm t ==> isVal t

-- Theorem 3.5.12: For every term t there is some normal form t' such that t -*> t'
prop_terminationOfEvaluation = forAll genTerm $ isNormalForm . eval

-- Page 39: Each evaluation step reduces the size of the term
prop_evaluationReducesTermSize = forAll genTerm $ \t -> maybeSize (eval1 t) < size t
  where maybeSize = maybe 0 size
