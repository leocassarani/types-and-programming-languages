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

index :: [a] -> Int -> Maybe a
index [] _ = Nothing
index (x:_) 0 = Just x
index (_:xs) i = index xs (i - 1)
