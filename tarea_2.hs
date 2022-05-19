{-# OPTIONS_GHC -fno-warn-tabs -fno-warn-overlapping-patterns #-}

-- Nicolás Andreoli - 210630

module Imp where

import Prelude

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
--ejec 1 paso

--Ej5
--ejec completa


--Ej6
--par
--suma
--largo
--igualdadN
--fibonacci
