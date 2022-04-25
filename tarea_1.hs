{-# OPTIONS_GHC -fno-warn-tabs -fno-warn-overlapping-patterns #-}

-- Nicolás Andreoli - 210630

module Chi where

import Prelude

-- 1

data Exp = Var String | Const String | Lambda [String] Exp | Aplic Exp [Exp] | Case Exp [Bs] | Rec String Exp
	deriving Show

type Bs = (String, [String], Exp)

-- 2

type Sigma = [(String, Exp)]

search :: (String, Sigma) -> Exp
search(x, s) = case lookup x s of {
	Just e -> e;
	Nothing -> Var x;
}

erease :: (Sigma, [String]) -> Sigma
erease(s, xs) = filter (\e -> notElem (fst(e)) xs) s

sustBs :: (Sigma, Bs) -> Bs
sustBs(s, (x, xs, e)) = (x, xs, sust(e, erease(s, xs)))

sustBsList :: (Sigma, [Bs]) -> [Bs]
sustBsList(z, []) = []
sustBsList(s, b:bs) = [sustBs(s, b)] ++ sustBsList(s, bs)

sust :: (Exp, Sigma) -> Exp
sust(Var x, s) = search(x,s)
sust(Const c, s) = Const c
sust(Lambda xs e, s) = Lambda xs (sust(e, erease(s, xs)))
sust(Aplic e es, s) = Aplic (sust(e, s)) es
sust(Case e t, s) = Case (sust(e, s)) (sustBsList(s, t))
sust(Rec x e, s) = Rec x (sust(e, s))


-- 3

errorMsg :: String
errorMsg = "La expresión no puede ser reducida"

eval :: (Exp) -> Exp
eval(Var x) = Var x
eval(Const c) = Const c
eval(Lambda xs e) = Lambda xs e
eval(Aplic e es) = evalAplic(e, es)
eval(Case e t) = evalCase(e, t)
eval(Rec x e) = eval(sust(e,[(x, Rec x e)]))

evalAplic :: (Exp, [Exp]) -> Exp
evalAplic(e, es) = case eval(e) of {
	Lambda xs u -> case length(xs) /= length(es) of {
		True -> error errorMsg;
		False -> eval(sust(u, zip xs (map eval es)));
	};
	Aplic (Const c) xs -> Aplic (Const c) (xs ++ (map eval es));
}

evalCase :: (Exp, [Bs]) -> Exp
evalCase(e, bs) = e
-- TODO