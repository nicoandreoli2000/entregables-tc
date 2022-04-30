{-# OPTIONS_GHC -fno-warn-tabs -fno-warn-overlapping-patterns #-}

-- Nicolás Andreoli - 210630

module Chi where

import Prelude hiding (and)


-- Ej 1

data Exp = Var String | Const String | Lambda [String] Exp | Aplic Exp [Exp] | Case Exp [Bs] | Rec String Exp
	deriving Show

type Bs = (String, [String], Exp)


-- Ej 2

type Sigma = [(String, Exp)]

search :: String -> Sigma -> Exp
search x s = case lookup x s of {
	Just e -> e;
	Nothing -> Var x;
}

erease :: Sigma -> [String] -> Sigma
erease s [] = s
erease s (y:ys) = erease (auxErease s y) ys
		
auxErease :: Sigma -> String -> Sigma 
auxErease [] x = []
auxErease ((s,e):xs) x = case x == s of {
	True -> xs;
	False -> (s,e):(auxErease xs x)
}

sustBs :: Sigma -> Bs -> Bs
sustBs [] b = b
sustBs ((s, e):xs) (y, ys, v) = sustBs xs (y, ys, sust v (erease [(s, e)] ys))

sustBsList :: Sigma -> [Bs] -> [Bs]
sustBsList s [] = []
sustBsList s (b:bs) = (sustBs s b):(sustBsList s bs)

sust :: Exp -> Sigma -> Exp
sust (Var x) s = search x s
sust (Const c) s = Const c
sust (Lambda xs e) s = Lambda xs (sust e (erease s xs))
sust (Aplic e es) s = Aplic (sust e s) (map (\x -> sust x s) es)
sust (Case e t) s = Case (sust e s) (sustBsList s t)
sust (Rec x e) s = Rec x (sust e s)

-- Ej 3

errorMsg :: String
errorMsg = "La expresión no puede ser reducida"

eval :: Exp -> Exp
eval (Var x) = error errorMsg
eval (Const c) = Aplic (Const c) []
eval (Lambda xs e) = Lambda xs e
eval (Aplic e es) = evalAplic e es
eval (Case e t) = evalCase e t
eval (Rec x e) = eval (sust e [(x, Rec x e)])

evalAplic :: Exp -> [Exp] -> Exp
evalAplic e es = case eval e of {
	Lambda xs u -> case length xs /= length es of {
		True -> error errorMsg;
		False -> eval (sust u (zip xs (map eval es)));
	};
	Aplic (Const c) xs -> Aplic (Const c) (xs ++ (map eval es));
	_ -> error errorMsg;
}

evalCase :: Exp -> [Bs] -> Exp
evalCase e bs = case eval e of {
	Aplic (Const c) xs -> case length xs /= length (snd (findExpCase bs c)) of {
		True -> error errorMsg;
		False -> eval (sust (fst (findExpCase bs c)) (zip (snd (findExpCase bs c)) xs));
	};
	_ -> error errorMsg;
}

findExpCase :: [Bs] -> String -> (Exp, [String])
findExpCase [] c = error errorMsg
findExpCase ((x, xs, e):bs) c = case x == c of {
	False -> findExpCase bs c;
	True -> (e, xs);
}


-- Ej 4

and :: Exp
and = Lambda ["x", "y"] (Case (Var "x") [
							("True", [], Var "y"),
							("False", [], Const "False")])
testAnd :: Exp
testAnd = eval (Aplic and [Const "True", Const "False"])

duplicar :: Exp
duplicar = Rec "Duplicar" (Lambda ["x"] (Case (Var "x") [
										("O", [], Const "O"),
										("S", ["y"], Aplic (Const "S") [Aplic (Const "S") [Aplic (Var "Duplicar") [Var "y"]]])
										]))

testDuplicar :: Exp
testDuplicar = eval (Aplic duplicar [Aplic (Const "S") [Const "O"]])

unir :: Exp
unir = Rec "Unir" (Lambda ["xs","ys"] (Case (Var "xs") [
									("[]", [], Var "ys"),
									(":", ["z", "zs"], Aplic (Const ":") [Var "z", Aplic (Var "Unir") [Var "zs", Var "ys"]])
									]))

testUnir :: Exp
testUnir = eval (Aplic unir [Aplic (Const ":") [Const "0", Aplic (Const ":") [Const "1", Const "[]"]],
							 Aplic (Const ":") [Const "2", Aplic (Const ":") [Const "3", Const "[]"]]])

auxRamaI :: Exp
auxRamaI = Rec "Aux" (Lambda ["x"] (Case (Var "x") [
										("Leaf", [], Const "[]"),
										("Tree", ["izq", "mid", "der"], Aplic (Const ":") [Var "mid", Aplic (Const ":") [Aplic (Var "Aux") [Var "izq"], Aplic (Var "Aux") [Var "der"]]])
										]))

ramaI :: Exp
ramaI = Lambda ["x"] (Case (Var "x") [
							("Leaf", [], Const "[]"),
							("Tree", ["izq", "mid", "der"] , Aplic (Const ":") [Var "mid", Aplic auxRamaI [Var "izq"]])
							])

testRamaI :: Exp
testRamaI = eval (Aplic ramaI [Aplic (Const "Tree") [
									Aplic (Const "Tree") [
													Const "Leaf",
													Const "2",
													Aplic (Const "Tree") [
																	Aplic (Const "Tree") [
																					Const "Leaf",
																					Const "4",
																					Const "Leaf"
																					],
																	Const "3",
																	Const "Leaf"
																	]
													],
									Const "1",
									Aplic (Const "Tree") [
													Const "Leaf",
													Const "99",
													Const "Leaf"
													]
									]])