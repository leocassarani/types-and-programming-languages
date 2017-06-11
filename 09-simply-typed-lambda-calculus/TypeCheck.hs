module TypeCheck where

import Lambda
import Control.Monad (guard)

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

index :: [a] -> Int -> Maybe a
index [] _ = Nothing
index (x:_) 0 = Just x
index (_:xs) i = index xs (i - 1)
