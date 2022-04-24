{-# OPTIONS_GHC -fno-warn-tabs -fno-warn-overlapping-patterns #-}

-- Nicolás Andreoli - 210630

module Evaluador where

import Prelude hiding (read)

-- 1) El orden de la evaluación importa, ya que
-- la evaluación de e1 es dado M y devuelve M', que luego
-- es usada inicialmente para evaluar e2 y devolver M''.
-- Por lo que al cambiar el orden de evaluación, cambiarían
-- las memorias iniciales que se usan para evaluar las expresiones
-- y el resultado podría ser distinto.


-- 2) 

type M = [(String, Integer)]

read :: (M, String) -> Integer

read ([], x) = error("La variable no se encuentra en la memoria")
read ((s, i): m, x) = case x == s of
							True -> i
							False -> read(m, x)




write :: (M, (String, Integer)) -> M

write([], (s, i)) = [(s, i)]
write((p@(s, i)): m, e@(x, y)) = case x == s of
									True -> e : m
									False -> p : write (m, e)



-- 3)

data Exp =  Var String | Lit Integer | Ass String Exp | Bin Op Exp Exp
	deriving (Show)
data Op =  (:+) | (:-) | (:*) | (:/) | (:%)
	deriving (Show)

eval :: (M, Exp) -> (M, Integer)

eval(m, Var x) = (m, read(m, x))
eval(m, Lit i) = (m, i)
eval(m, Ass x e) = (write(fst(eval(m, e)), (x, snd(eval(m, e)))), snd(eval(m, e)))
eval(m, Bin (:+) e1 e2) = (fst(eval(fst(eval(m, e1)), e2)), snd(eval(m, e1)) + snd(eval(fst(eval(m, e1)), e2)))
eval(m, Bin (:-) e1 e2) = (fst(eval(fst(eval(m, e1)), e2)), snd(eval(m, e1)) - snd(eval(fst(eval(m, e1)), e2)))
eval(m, Bin (:*) e1 e2) = (fst(eval(fst(eval(m, e1)), e2)), snd(eval(m, e1)) * snd(eval(fst(eval(m, e1)), e2)))
eval(m, Bin (:/) e1 e2) = (fst(eval(fst(eval(m, e1)), e2)), snd(eval(m, e1)) `div` snd(eval(fst(eval(m, e1)), e2)))
eval(m, Bin (:%) e1 e2) = (fst(eval(fst(eval(m, e1)), e2)), snd(eval(m, e1)) `mod` snd(eval(fst(eval(m, e1)), e2)))




-- Testing

x :: String
x = "x"

y :: String
y = "y"

z :: String
z = "z"

m1 :: M
m1 = []

m2 :: M
m2 = [(x,1)]

-- Run this on the terminal
eval(m1,e1)
eval(m1,e2)
eval(m1,e3)
eval(m1,e4)
eval(m1,e5)
eval(m2,e6)

e1 :: Exp
e1 = Lit 13
-- *Evaluador> eval(m1,e1)
-- ([],13)

e2 :: Exp
e2 = Var x
-- *Evaluador> eval(m1,e2)
-- ([],*** Exception: La variable no se encuentra en la memoria
-- CallStack (from HasCallStack):
--   error, called at tarea_0.hs:15:16 in main:Evaluador

e3 :: Exp
e3 = Bin (:+) (Ass x (Lit 100)) (Bin (:*) (Var x) (Var x))
-- *Evaluador> eval(m1,e3)
-- ([("x",100)],10100)

e4 :: Exp
e4 = Ass x (Ass y (Lit 1))
-- *Evaluador> eval(m1,e4)
-- ([("y",1),("x",1)],1)



e5 :: Exp
e5 = Bin (:+) (Ass x (Lit 0)) (Bin (:/) (Ass y (Lit 1)) (Var x))
-- *Evaluador> eval(m1,e5)
-- ([("x",0),("y",1)],*** Exception: divide by zero



e6 :: Exp
e6 = Bin (:+) (Ass x (Lit 100)) (Ass y (Bin (:+) (Var x) (Var x)))
-- *Evaluador> eval(m2,e6)
-- ([("x",100),("y",200)],300)
-- *Evaluador>
