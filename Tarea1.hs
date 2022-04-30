-- Martina Cantera (256233)

{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Tarea1 where

data E = Var String | C String | L [String] E | Ap E [E] | Case E [B] | Rec String E
 deriving Show

type B = (String,([String],E))

type Sust = [(String,E)]

-- 2)eval(Ap duplicar [Ap (C "S") [C "O"]])
sust :: Sust -> E -> E 
sust s (Var x) = case (lookup x s) of {
					Just n -> n ;
					Nothing -> Var x
					};
sust s (C c) = C c
sust s (L x e) = L (x) (sust (bajas s x) e)
sust s (Ap e es) = Ap (sust s e) (map (sust s) es)
sust s (Case e b) = Case (sust s e) (sustRamas s b)
sust s (Rec x e) = Rec (x) (sust (baja s x) e)


bajas:: Sust -> [String] -> Sust 
bajas s [] = s
bajas s (y:ys) = bajas (baja s y) ys
		
baja :: Sust -> String -> Sust 
baja [] x = []
baja ((s,e):xs) x
			| (x==s) = xs
			| otherwise = (s,e):(baja xs x)


sustRamas :: Sust -> [B] -> [B]
sustRamas x [] = [] 
sustRamas s (b:bs) = (sustRama s b):(sustRamas s bs)

sustRama :: Sust -> B -> B
sustRama [] b = b
sustRama ((s,e):xs) (a,(c,e1)) =  sustRama xs (a,(c,(sust (bajas [(s,e)] c) e1)))

	
-- 3)

eval :: E -> E	
eval (Var x) = error "error de tiempo de ejecución"		
eval (C c) = Ap (C c) []
eval (L x e) = L x e 
eval (Ap e es) = case (eval e) of{
						(Ap (C c) vs) -> Ap (C c) (vs ++ (map eval es));
						(L xs e1) -> case (length(xs)== length(map eval es)) of {
												True -> eval (sust (zip xs (map eval es)) e1);
												False -> error "error de tiempo de ejecución";
												};
						otherwise -> error "error de tiempo de ejecución";
						};
eval (Case e bs) = case (eval e) of{ 
							(Ap (C c) vs) -> case (lookup c bs) of {
													Just (xs,e1) -> case (length(xs)==length(vs)) of {
																			True -> eval (sust (zip xs vs) e1);
																			False -> error "error de tiempo de ejecución";
																			};
													Nothing -> error "error de tiempo de ejecución";
													};
							otherwise -> error "error de tiempo de ejecución";
							};
eval (Rec x e) = eval (sust [(x,(Rec x e))] e)


andChi :: E
andChi = L ["x","y"] (Case (Var "x") [
					("True" , ([] , (Case (Var "y") [
										("True" , ([] , (C "True"))),
										("False" , ([] , (C "False")))
										]))),
					("False" , ([] , (C "False")))
					])

duplicar :: E
duplicar =Rec "*2" (L ["n"] (Case (Var "n")[
						("O" , ([] , (C "O"))),
						("S" , (["x"] , (Ap (C "S") [Ap (C "S") [Ap (Var "*2") [Var "x"]]])))
						]))

-- unir :: E 
-- unir = Rec "++" (L ["xs","ys"] (Case (Var "xs") [
-- 									("[]" , ([] , (Var "ys"))),
-- 									(":" , (["z","zs"] , (Case (Var "ys") [
-- 																("[]" , ([] , (Var "xs"))),
-- 																(":" , (["w","ws"] , (Ap (C ":") [(Var "z"),(Ap (C ":") [(Var "w"),(Ap (Var "++") [(Var "zs"),(Var "ws")])])])))
-- 																])))
-- 									]))

-- ramaI :: E
-- ramaI = Rec "ri" (L ["a"] (Case (Var "a") [
-- 								("H" , ([] , (C "[]"))),
-- 								("N" , (["ai","d","ad"] , (Ap (C ":") [(Var "d") , (Ap (Var "ri") [Var "ai"])])))

