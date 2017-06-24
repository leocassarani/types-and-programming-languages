{-# LANGUAGE TupleSections #-}

module TypeCheck where

import Lambda
import Control.Monad (guard)
import Data.List (find)

typeOf :: Context -> Term -> Maybe Type

typeOf _ Tru = return Bool -- T-True

typeOf _ Fls = return Bool -- T-False

typeOf _ Unit = return UnitType -- T-Unit

typeOf ctx (If t1 t2 t3) = do -- T-If
  Bool <- typeOf ctx t1
  typ2 <- typeOf ctx t2
  typ3 <- typeOf ctx t3
  guard (typ2 == typ3)
  return typ2

typeOf ctx (Var x) = do -- T-Var
  (_, VarBind typ) <- ctx `index` x
  return typ

typeOf ctx (Abs x typ1 t1) = do -- T-Abs
  let ctx' = (x, VarBind typ1) : ctx
  typ2 <- typeOf ctx' t1
  return (Func typ1 typ2)

typeOf ctx (App t1 t2) = do -- T-App
  Func typ11 typ12 <- typeOf ctx t1
  typ2 <- typeOf ctx t2
  guard (typ11 == typ2)
  return typ12

typeOf ctx (As t1 typ1) = do -- T-Ascribe
  typ2 <- typeOf ctx t1
  guard (typ1 == typ2)
  return typ1

typeOf ctx (Let x t1 t2) = do -- T-Let
  typ1 <- typeOf ctx t1
  let ctx' = (x, VarBind typ1) : ctx
  typeOf ctx' t2

typeOf ctx (Tuple terms) = -- T-Tuple
  TupleType <$> sequence (map (typeOf ctx) terms)

typeOf ctx (TupleProject t1 idx) = do --T-Proj
  TupleType types <- typeOf ctx t1
  types `index` (idx - 1)

typeOf ctx (Record entries) = -- T-Rcd
  let entryType (l, t) = (l,) <$> typeOf ctx t
   in RecordType <$> sequence (map entryType entries)

typeOf ctx (RecordProject t1 label) = do -- T-Proj
  RecordType entries <- typeOf ctx t1
  snd <$> find ((label ==) . fst) entries

typeOf ctx (Tag label term typ@(Variant vars)) = do -- T-Variant
  (_, varType) <- find ((label ==) . fst) vars
  termType <- typeOf ctx term
  guard (varType == termType)
  return typ

typeOf ctx (Case t0 branches) = do -- T-Case
  Variant vars <- typeOf ctx t0
  guard (length vars == length branches)
  typ : types <- sequence (zipWith (branchType ctx) vars branches)
  guard (all (typ ==) types)
  return typ

typeOf _ (Nil typ) = return (List typ) -- T-Nil

typeOf ctx (Cons typ t1 t2) = do -- T-Cons
  headType <- typeOf ctx t1
  guard (headType == typ)
  (List tailType) <- typeOf ctx t2
  guard (tailType == typ)
  return (List typ)

typeOf ctx (IsNil typ t1) = do -- T-IsNil
  (List termType) <- typeOf ctx t1
  guard (termType == typ)
  return Bool

typeOf ctx (Head typ t1) = do -- T-Head
  (List termType) <- typeOf ctx t1
  guard (termType == typ)
  return typ

typeOf ctx (Tail typ t1) = do -- T-Tail
  (List termType) <- typeOf ctx t1
  guard (termType == typ)
  return (List typ)

branchType :: Context -> (String, Type) -> (String, String, Term) -> Maybe Type
branchType ctx (vlab, typ) (blab, x, term) = do
  guard (vlab == blab)
  typeOf ((x, VarBind typ) : ctx) term

index :: [a] -> Int -> Maybe a
index [] _ = Nothing
index (x:_) 0 = Just x
index (_:xs) i = index xs (i - 1)
