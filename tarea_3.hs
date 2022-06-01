{-# OPTIONS_GHC -fno-warn-tabs -fno-warn-overlapping-patterns #-}

-- Nicolás Andreoli - 210630

module Turing where

import Prelude hiding (init)

--Ej1
type Symbol = String
blank :: Symbol
blank = "#"
type Tape = ([Symbol],Symbol,[Symbol])

--Ej2
data Action = L | R | O Symbol
                deriving Show
type State = String
init :: State
init = "i"
halt :: State
halt = "h"
type Code = [(State,Symbol,Action,State)]

--Ej3
type Config = (State,Tape)

--Ej4
err :: String
err = "error en tiempo de ejecución"

lookupTuring :: State -> Symbol -> Code -> (Action,State)
lookupTuring q x [] = error err
lookupTuring q x ((q',x',a,s):cs) = case (q' == q && x' == x) of {
    True -> (a,s);
    False -> lookupTuring q x cs;
}

step :: Code -> Config -> Config
step c (q,(l:ls,x,r:rs)) = case a of {
    R -> (s,(ls,l,x:r:rs));
    L -> (s,(x:l:ls,r,rs));
    O z -> (s,(l:ls,z,r:rs));
} where {
    (a,s) = lookupTuring q x c;
}


--Ej5
execWithConfig :: Code -> Config -> Tape
execWithConfig code config@(q,t) = case q == halt of {
    True -> t;
    False -> execWithConfig code (step code config);
}

exec :: Code -> Tape -> Tape
exec c t = execWithConfig c (init,t)


--Ej6

par :: Code
par = [
        (init,blank,L,"reversa"),
        ("reversa","I",L,"reversa"),("reversa",blank,R,"par"),
        ("par","I",R,"impar"),("impar","I",R,"par"),
        ("par",blank,R,"movPar"),("movPar",blank,L,"T"),
        ("impar",blank,R,"movImpar"),("movImpar",blank,L,"F"),
        ("T",blank,O "T","fin"),("F",blank,O "F","fin"),
        ("fin","T",R,halt),("fin","F",R,halt)

    ]

cintaPar :: Tape
cintaPar = (["I","I","I","I",blank],blank,[blank,blank])

cintaImpar :: Tape
cintaImpar = (["I","I","I","I","I",blank],blank,[blank,blank])

-- shift-right
-- reverse