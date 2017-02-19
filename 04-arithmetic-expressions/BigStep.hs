module BigStep where

import Arith
import qualified SmallStep (eval)
import Test.QuickCheck

isNormalForm :: Term -> Bool
isNormalForm t = eval t == t

eval :: Term -> Term

eval t@(TmIf t1 t2 t3) =
  case eval t1 of
    TmTrue -> eval t2                 -- B-IfTrue
    TmFalse -> eval t3                -- B-IfFalse
    otherwise -> t                    -- No reduction

eval t@(TmSucc t1)
  | isNumericVal t1' = TmSucc t1'     -- B-Succ
  | otherwise = t                     -- No reduction
  where t1' = eval t1

eval t@(TmPred t1) =
  case eval t1 of
    TmZero -> TmZero                  -- B-PredZero
    (TmSucc nv) -> if isNumericVal nv -- B-PredSuc
                      then nv
                      else t
    otherwise -> t                    -- No reduction

eval t@(TmIsZero t1) =
  case eval t1 of
    TmZero -> TmTrue                  -- B-IszeroZero
    (TmSucc nv) -> if isNumericVal nv -- B-IszeroSucc
                      then TmFalse
                      else t
    otherwise -> t                    -- No reduction

eval t
  | isVal t = t                       -- B-Value
  | otherwise = t                     -- No reduction

-- Exercise 3.5.17: Show that small-step and big-step semantics for this language coincide,
-- i.e. t-*> v iff t ⇓ v
prop_bigAndSmallStepEquivalence t =
  let v1 = eval t
      v2 = SmallStep.eval t
   in (isVal v1 ==> v1 == v2) .&&. (isVal v2 ==> v1 == v2)

main = quickCheckWith stdArgs { maxSuccess = 100000 } prop_bigAndSmallStepEquivalence
