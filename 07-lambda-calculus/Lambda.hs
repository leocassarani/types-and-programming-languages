module Lambda where

data Term = Var Int
          | Abs String Term
          | App Term Term
          deriving (Eq)

-- λx. x
tmId = Abs "x" (Var 0)

-- λt. λf. t
tmTru = Abs "t" (Abs "f" (Var 1))
-- λt. λf. f
tmFls = Abs "t" (Abs "f" (Var 0))
-- λl. λm. λn. l m n
tmTest = Abs "l" (Abs "m" (Abs "n" (App (App (Var 2) (Var 1)) (Var 0))))

-- λb. λc. b c tru
tmAnd = Abs "b" (Abs "c" (App (App (Var 1) (Var 0)) tmFls))
-- λb. λc. b tru c
tmOr = Abs "b" (Abs "c" (App (App (Var 1) tmTru) (Var 0)))
-- λb. b fls tru
tmNot = Abs "b" (App (App (Var 0) tmFls) tmTru)

-- λf. λs. λb. b f s
tmPair = Abs "f" (Abs "s" (Abs "b" (App (App (Var 0) (Var 2)) (Var 1))))
-- λp. p tru
tmFst = Abs "p" (App (Var 0) tmTru)
-- λp. p fls
tmSnd = Abs "p" (App (Var 0) tmFls)

-- λs. λz. z
tmZero = Abs "s" (Abs "z" (Var 0))
-- λm. m (λx. fls) tru
tmIsZero = Abs "m" (App (App (Var 0) (Abs "x" tmFls)) tmTru)

-- λn. λs. λz. s (n s z)
tmSucc = Abs "n" (Abs "s" (Abs "z" (App (Var 1) (App (App (Var 2) (Var 1)) (Var 0)))))
-- λm. fst (m ss zz)
tmPred = Abs "m" (App tmFst (App (App (Var 0) tmSs) tmZz))
  where tmZz = App (App tmPair tmZero) tmZero
        tmSs = Abs "p" (App (App tmPair (App tmSnd (Var 0))) (App (App tmPlus tmOne) (App tmSnd (Var 0))))
        tmOne = App tmSucc tmZero

-- λm. λn. λs. λz. m s (n s z)
tmPlus = Abs "m" (Abs "n" (Abs "s" (Abs "z" (App (App (Var 3) (Var 1)) (App (App (Var 2) (Var 1)) (Var 0))))))
-- λm. λn. n pred m
tmSub = Abs "m" (Abs "n" (App (App (Var 0) tmPred) (Var 1)))
-- λm. λn. m (plus n) zero
tmTimes = Abs "m" (Abs "n" (App (App (Var 1) (App tmPlus (Var 0))) tmZero))
-- λm. λn. m n
tmPow = Abs "m" (Abs "n" (App (Var 1) (Var 0)))

-- λm. λn. and (isZero (sub m n)) (isZero (sub n m))
tmEqual = Abs "m" (Abs "n" (App (App tmAnd (App tmIsZero (App (App tmSub (Var 1)) (Var 0)))) (App tmIsZero (App (App tmSub (Var 0)) (Var 1)))))

instance Show Term where
  show = showTm []

type Context = [String]

showTm :: Context -> Term -> String
showTm ctx (Var x) = indexToName ctx x
showTm ctx (App t1 t2) = "(" ++ showTm ctx t1 ++ " " ++ showTm ctx t2 ++ ")"
showTm ctx (Abs x t1) = "(λ" ++ x' ++ ". " ++ showTm ctx' t1 ++ ")"
  where (ctx', x') = freshName ctx x

indexToName :: Context -> Int -> String
indexToName ctx x
  | x < length ctx = ctx !! x
  | otherwise = "[" ++ show x ++ "]"

freshName :: Context -> String -> (Context, String)
freshName ctx x
  | x `elem` ctx = freshName ctx (x ++ "'")
  | otherwise = (x : ctx, x)

termShift :: Int -> Term -> Term
termShift d = termShift' 0
  where
    termShift' c t@(Var x)
      | x >= c = Var (x + d)
      | otherwise = t
    termShift' c (Abs x t1) = Abs x (termShift' (c + 1) t1)
    termShift' c (App t1 t2) = App (termShift' c t1) (termShift' c t2)

termSub :: Int -> Term -> Term -> Term
termSub j s = termSub' 0
  where
    termSub' c t@(Var x)
      | x == j + c = termShift c s
      | otherwise = t
    termSub' c (Abs x t1) = Abs x (termSub' (c + 1) t1)
    termSub' c (App t1 t2) = App (termSub' c t1) (termSub' c t2)

termSubTop :: Term -> Term -> Term
termSubTop s t = termShift (-1) (termSub 0 (termShift 1 s) t)

isVal :: Term -> Bool
isVal (Abs _ _) = True
isVal _ = False

eval1 :: Term -> Maybe Term
eval1 (App (Abs x t12) v2)
  | isVal v2 = Just (termSubTop v2 t12) -- E-AppAbs
eval1 (App t1 t2)
  | isVal t1  = fmap (\t2' -> App t1 t2') (eval1 t2) -- E-App2
  | otherwise = fmap (\t1' -> App t1' t2) (eval1 t1) -- E-App1
eval1 _ = Nothing

eval :: Term -> Term
eval t = maybe t eval (eval1 t)
