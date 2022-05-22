{-# OPTIONS_GHC -fno-warn-tabs -fno-warn-overlapping-patterns #-}

-- Nicolás Andreoli - 210630

module Imp where

import Prelude hiding (not)

--Ej 1,2,3

type Rama = (String,([String],Prog))
type Mem = [(String,Val)] 
type Prog = [Instr]

data Val = ConstVal String [Val]
	deriving Show
data Instr = Asign [(String,Exp)] | Case String [Rama] | While String [Rama]
	deriving Show
data Exp = Var String | ConstExp String [Exp] 
	deriving Show	

lookupImp :: Mem -> String -> Val
lookupImp m x = case lookup x m of {
	Just y -> y;
	Nothing -> error "La variable no se encuentra en la memoria"
}

update :: Mem -> [(String,Val)] -> Mem
update m [] = m
update m ((x,v):a) = update (write m (x,v)) a  

write :: Mem -> (String,Val) -> Mem
write [] (x,v) = [(x,v)]
write ((y,u):a) (x,v) = case y == x of {
	True -> (x,v):a;
	False -> (y,u):(write a (x,v));
}

eval :: Mem -> Exp -> Val
eval m (Var x) = lookupImp m x
eval m (ConstExp c e) = ConstVal c (map (eval m) e)

--Ej4
ejec :: Mem -> Prog -> (Mem, Prog)
ejec m [] = (m, [])
ejec m ((Asign p):ps) = (update m (zip (map fst p) (map (eval m) (map snd p))), ps)
ejec m ((Case x bs):ps) = (update m (zip xs vs), p++ps) where {
	ConstVal c vs = lookupImp m x;
	(xs, p) = case lookup c bs of {
		Just t -> t;
		Nothing -> error "El constructor no se encuentra en las ramas";
	};
}
ejec m ((While x bs):ps) = case lookup c bs of {
	Just t -> (update m (zip xs vs), p++((While x bs):ps)) where {
		(xs, p) = case lookup c bs of {
			Just e -> e;
			Nothing -> error "El constructor no se encuentra en las ramas";
		};
	};
	Nothing -> (m, ps);
} where {
	ConstVal c vs = lookupImp m x;
}

--Ej5
ejecTotal :: Mem -> Prog -> Mem
ejecTotal [] p = []
ejecTotal n t = ejecTotal m p where {
	(m, p) = ejec n t
}


--Ej6
m :: Mem
m = [("x", ConstVal "True" [])]

--not
not :: Prog
not = [Case "x" [
		("True", ([], [Asign [("x", ConstExp "False" [])]])),
		("False", ([], [Asign [("x", ConstExp "True" [])]]))
		]
	]
--par
-- par :: Prog
-- par = [ 
-- 	Assign [("esPar", false),("n", Var "x")],
-- 	While "n" ["S",("y", [Assign [("n", Var "y")]],)]
-- 		]

--suma
--largo
--igualdadN
--fibonacci