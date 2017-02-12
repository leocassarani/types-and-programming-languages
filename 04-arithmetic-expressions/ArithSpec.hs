import Arith
import Control.Monad (liftM, liftM3)
import Data.Maybe (isNothing)
import Test.QuickCheck

instance Arbitrary Term where
  arbitrary = genTerm

genTerm = oneof [return TmTrue,
                 return TmFalse,
                 return TmZero,
                 liftM TmSucc genTerm,
                 liftM TmPred genTerm,
                 liftM TmIsZero genTerm,
                 liftM3 TmIf genTerm genTerm genTerm]

prop_everyValueNormalForm t = isVal t ==> isNormalForm t
  where isNormalForm = isNothing . eval1

main = quickCheck prop_everyValueNormalForm
