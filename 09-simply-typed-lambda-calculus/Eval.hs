{-# LANGUAGE TupleSections #-}

module Eval where

import Data.List (find, findIndex)

import Lambda

termShift :: Int -> Term -> Term
termShift d = termShift' 0
  where
    termShift' c t@(Var x)
      | x >= c = Var (x + d)
      | otherwise = t
    termShift' c (Abs x typ t1) = Abs x typ (termShift' (c + 1) t1)
    termShift' c (App t1 t2) = App (termShift' c t1) (termShift' c t2)
    termShift' c (If t1 t2 t3) = If (termShift' c t1) (termShift' c t2) (termShift' c t3)
    termShift' _ t = t

termSub :: Int -> Term -> Term -> Term
termSub j s = termSub' 0
  where
    termSub' c t@(Var x)
      | x == j + c = termShift c s
      | otherwise = t
    termSub' c (Abs x typ t1) = Abs x typ (termSub' (c + 1) t1)
    termSub' c (App t1 t2) = App (termSub' c t1) (termSub' c t2)
    termSub' c (If t1 t2 t3) = If (termSub' c t1) (termSub' c t2) (termSub' c t3)
    termSub' _ t = t

termSubTop :: Term -> Term -> Term
termSubTop s t = termShift (-1) (termSub 0 (termShift 1 s) t)

eval1 :: Term -> Maybe Term

eval1 (If Tru t _) = Just t -- E-IfTrue
eval1 (If Fls _ t) = Just t -- E-IfFalse

eval1 (If t1 t2 t3) = (\t1' -> If t1' t2 t3) <$> eval1 t1 -- E-If

eval1 (App (Abs _ _ t12) v2)
  | isVal v2 = Just (termSubTop v2 t12) -- E-AppAbs

eval1 (App t1 t2)
  | isVal t1 = App t1 <$> eval1 t2 -- E-App2
  | otherwise = (\t1' -> App t1' t2) <$> eval1 t1 -- E-App1

eval1 (As t1 typ)
  | isVal t1 = Just t1 -- E-Abscribe
  | otherwise = (\t1' -> As t1' typ) <$> eval1 t1 -- E-Ascribe1

eval1 (Let x t1 t2)
  | isVal t1 = Just (termSubTop t1 t2) -- E-LetV
  | otherwise = (\t1' -> Let x t1' t2) <$> eval1 t1 -- E-Let

eval1 (Tuple terms) = do -- E-Tuple
  idx <- findIndex (not . isVal) terms
  term <- terms `index` idx
  fmap (Tuple . replace terms idx) (eval1 term)

eval1 (TupleProject t@(Tuple terms) idx)
  | isVal t = terms `index` (idx - 1) -- E-ProjTuple

eval1 (TupleProject t idx) = (\t' -> TupleProject t' idx) <$> eval1 t -- E-Proj

eval1 (Record entries) = do --E-Rcd
  idx <- findIndex (not . isVal . snd) entries
  (label, term) <- entries `index` idx
  entry' <- (label,) <$> eval1 term
  let entries' = replace entries idx entry'
  return (Record entries')

eval1 (RecordProject r@(Record entries) label)
  | isVal r = snd <$> lookup entries label -- E-ProjRcd
  | otherwise = (\r' -> RecordProject r' label) <$> eval1 r -- E-Proj
  where lookup entries label = find ((label ==) . fst) entries

eval1 _ = Nothing

index :: [a] -> Int -> Maybe a
index [] _ = Nothing
index (x:_) 0 = Just x
index (_:xs) i = index xs (i - 1)

replace :: [a] -> Int -> a -> [a]
replace xs n x = take n xs ++ [x] ++ drop (n + 1) xs

eval :: Term -> Term
eval t = maybe t eval (eval1 t)
