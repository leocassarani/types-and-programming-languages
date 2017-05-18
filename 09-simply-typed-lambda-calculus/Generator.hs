module Generator where

import Lambda

import Control.Monad (liftM, liftM2)
import Data.List (elemIndices)
import Test.QuickCheck

instance Arbitrary Type where
  arbitrary = sized genType

genType :: Int -> Gen Type
genType 0 = return Bool
genType n = do
  n'  <- choose (0, n `div` 2)
  n'' <- choose (0, n `div` 2)
  liftM2 Func (genType n') (genType n'')

instance Arbitrary Term where
  arbitrary = genTerm [] =<< arbitrary

genTerm :: [Type] -> Type -> Gen Term
genTerm ctx typ
  | null vars = genTerm' ctx typ
  | otherwise = frequency [ (5, liftM Var (elements vars))
                          , (1, genTerm' ctx typ)
                          ]
    where vars = elemIndices typ ctx

genTerm' :: [Type] -> Type -> Gen Term
genTerm' ctx Bool = frequency [ (9, elements [Tru, Fls])
                              , (2, genIf ctx Bool)
                              , (1, genApp ctx Bool)
                              ]
genTerm' ctx typ@(Func typ1 typ2) = frequency [ (6, genAbs ctx typ1 typ2)
                                              , (2, genIf ctx typ)
                                              , (1, genApp ctx typ)
                                              ]

genIf :: [Type] -> Type -> Gen Term
genIf ctx typ = do
  cond <- genTerm ctx Bool
  thenBranch <- genTerm ctx typ
  elseBranch <- genTerm ctx typ
  return (If cond thenBranch elseBranch)

genAbs :: [Type] -> Type -> Type -> Gen Term
genAbs ctx typ1 typ2 = do
  let ctx' = typ1 : ctx
  body <- genTerm ctx' typ2
  name <- varName
  return (Abs name typ1 body)

-- Generate a random one-letter variable name.
varName :: Gen String
varName = do
  char <- elements ['a'..'z']
  return [char]

genApp :: [Type] -> Type -> Gen Term
genApp ctx typ = do
  argTyp <- genTypeFromCtx ctx
  arg <- genTerm ctx argTyp
  func <- genTerm ctx (Func argTyp typ)
  return (App func arg)

-- If we have any types in the environment, return one of them instead of
-- generating a brand new one. This helps with finding suitable arguments
-- for randomly-generated App terms.
genTypeFromCtx :: [Type] -> Gen Type
genTypeFromCtx [] = arbitrary
genTypeFromCtx ctx = elements ctx
