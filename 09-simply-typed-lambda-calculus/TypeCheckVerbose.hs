{-# LANGUAGE FlexibleInstances #-}

module TypeCheck where

import Lambda
import qualified Control.Monad.Fail as Fail

instance Fail.MonadFail (Either String) where
  fail = Left

typeOf :: Context -> Term -> Either String Type

typeOf _ Tru = return Bool
typeOf _ Fls = return Bool

typeOf ctx (If t1 t2 t3) = do
  typ1 <- typeOf ctx t1
  typ2 <- typeOf ctx t2
  typ3 <- typeOf ctx t3
  if typ1 == Bool
     then if typ2 == typ3
             then return typ2
             else Left "branch mismatch"
     else Left "condition must be a Bool"

typeOf ctx (Var x) = do -- T-Var
  (_, VarBind typ) <- ctx `index` x
  return typ

typeOf ctx (App t1 t2) = do -- T-App
  Func typ11 typ12 <- typeOf ctx t1
  typ2 <- typeOf ctx t2
  if (typ11 == typ2)
     then return typ12
     else fail "function argument mismatch"

index :: [a] -> Int -> Either String a
index [] _ = Left "index out of bounds"
index (x:_) 0 = return x
index (_:xs) i = index xs (i - 1)
