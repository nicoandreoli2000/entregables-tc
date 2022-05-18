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

--eval

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
