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
type Code = [((State,Symbol),(Action,State))]

--Ej3
type Config = (State,Tape)

--Ej4
err :: String
err = "error en tiempo de ejecución"

step :: Code -> Config -> Config
step c (q,t@(l,x,r)) = case lookup (q,x) c of {
    Just (a,s) -> case a of {
        L -> (s, movLeft t);
        R -> (s, movRight t);
        O z -> (s,(l,z,r));
    };
    Nothing -> error err;
}

movLeft :: Tape -> Tape
movLeft ([],x,r) = error err
movLeft (l:ls,x,r) = (ls,l,x:r)

movRight :: Tape -> Tape
movRight (l,x,[]) = error err
movRight (l,x,r:rs) = (x:l,r,rs)

--Ej5
execWithConfig :: Code -> Config -> Tape
execWithConfig code config@(q,t) = case q == halt of {
    True -> t;
    False -> execWithConfig code (step code config);
}

exec :: Code -> Tape -> Tape
exec c t = execWithConfig c (init,t)


--Ej6

cintaCasoBorde :: Tape
cintaCasoBorde = ([blank],blank,[blank,blank])

cintaPar :: Tape
cintaPar = (["I","I","I","I",blank],blank,[blank,blank])

cintaImpar :: Tape
cintaImpar = (["I","I","I","I","I",blank],blank,[blank,blank])

par :: Code
par = [
        ((init,blank),(L,"reversa")),
        (("reversa","I"),(L,"reversa")),
        (("reversa",blank),(R,"par")),
        (("par","I"),(R,"impar")),
        (("impar","I"),(R,"par")),
        (("par",blank),(R,"T")),
        (("T",blank),(O "T","fin")),
        (("impar",blank),(R,"F")),
        (("F",blank),(O "F","fin")),
        (("fin","T"),(R,halt)),
        (("fin","F"),(R,halt))
    ]

cintaShiftRight :: Tape
cintaShiftRight = (["X","Y","X","Y","Z","X","Z",blank],blank,[blank])

shiftRight :: Code
shiftRight = [
        ((init,blank),(L,"reversa")),
        (("reversa","X"),(O blank, "limpiarX")),
        (("reversa","Y"),(O blank, "limpiarY")),
        (("reversa","Z"),(O blank, "limpiarZ")),
        (("limpiarX",blank),(R, "escribirX")),
        (("limpiarY",blank),(R, "escribirY")),
        (("limpiarZ",blank),(R, "escribirZ")),
        (("escribirX","X"),(R, "escribirX")),
        (("escribirY","Y"),(R, "escribirY")),
        (("escribirZ","Z"),(R, "escribirZ")),
        (("escribirX",blank),(O "X", "vuelta")),
        (("escribirY",blank),(O "Y", "vuelta")),
        (("escribirZ",blank),(O "Z", "vuelta")),
        (("vuelta","X"),(L,"vuelta")),
        (("vuelta","Y"),(L,"vuelta")),
        (("vuelta","Z"),(L,"vuelta")),
        (("vuelta",blank),(L,"reversa")),
        (("reversa",blank),(R,"ultimo")),
        (("ultimo",blank),(R,"fin")),
        (("fin","X"),(R,"fin")),
        (("fin","Y"),(R,"fin")),
        (("fin","Z"),(R,"fin")),
        (("fin",blank),(O blank,halt))
    ]

-- exec shiftRight cintaShiftRight
-- reverse