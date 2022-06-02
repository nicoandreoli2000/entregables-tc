{-# OPTIONS_GHC -fno-warn-tabs -fno-warn-overlapping-patterns #-}

-- Nicolás Andreoli - 210630

module Turing where

import Prelude hiding (init,reverse)

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

cintaParCasoBorde :: Tape
cintaParCasoBorde = ([blank],blank,[blank,blank])

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

cintaShiftRightCasoBorde :: Tape
cintaShiftRightCasoBorde = ([blank],blank,[blank])

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

cintaReverseCasoBorde :: Tape
cintaReverseCasoBorde = ([blank],blank,[blank])

cintaReverse :: Tape
cintaReverse = (["Y","X","X","Y","Z","Z","X",blank],blank,[blank,blank,blank,blank,blank,blank,blank,blank])

reverse :: Code
reverse = [
        ((init,blank),(L,"reversaFinal")),
        (("reversaFinal","X"),(O "@","marchaAdelante1X")),
        (("reversaFinal","Y"),(O "@","marchaAdelante1Y")),
        (("reversaFinal","Z"),(O "@","marchaAdelante1Z")),
        (("marchaAdelante1X","@"),(R,"marchaAdelante1X")),
        (("marchaAdelante1Y","@"),(R,"marchaAdelante1Y")),
        (("marchaAdelante1Z","@"),(R,"marchaAdelante1Z")),
        (("marchaAdelante1X","X"),(R,"marchaAdelante1X")),
        (("marchaAdelante1X","Y"),(R,"marchaAdelante1X")),
        (("marchaAdelante1X","Z"),(R,"marchaAdelante1X")),
        (("marchaAdelante1Y","X"),(R,"marchaAdelante1Y")),
        (("marchaAdelante1Y","Y"),(R,"marchaAdelante1Y")),
        (("marchaAdelante1Y","Z"),(R,"marchaAdelante1Y")),
        (("marchaAdelante1Z","X"),(R,"marchaAdelante1Z")),
        (("marchaAdelante1Z","Y"),(R,"marchaAdelante1Z")),
        (("marchaAdelante1Z","Z"),(R,"marchaAdelante1Z")),
        (("marchaAdelante1X",blank),(R,"marchaAdelante2X")),
        (("marchaAdelante1Y",blank),(R,"marchaAdelante2Y")),
        (("marchaAdelante1Z",blank),(R,"marchaAdelante2Z")),
        (("marchaAdelante2X","X"),(R,"marchaAdelante2X")),
        (("marchaAdelante2X","Y"),(R,"marchaAdelante2X")),
        (("marchaAdelante2X","Z"),(R,"marchaAdelante2X")),
        (("marchaAdelante2Y","X"),(R,"marchaAdelante2Y")),
        (("marchaAdelante2Y","Y"),(R,"marchaAdelante2Y")),
        (("marchaAdelante2Y","Z"),(R,"marchaAdelante2Y")),
        (("marchaAdelante2Z","X"),(R,"marchaAdelante2Z")),
        (("marchaAdelante2Z","Y"),(R,"marchaAdelante2Z")),
        (("marchaAdelante2Z","Z"),(R,"marchaAdelante2Z")),
        (("marchaAdelante2X",blank),(O "X","reversa2X")),
        (("marchaAdelante2Y",blank),(O "Y","reversa2Y")),
        (("marchaAdelante2Z",blank),(O "Z","reversa2Z")),
        (("reversa2X","X"),(L,"reversa2X")),
        (("reversa2X","Y"),(L,"reversa2X")),
        (("reversa2X","Z"),(L,"reversa2X")),
        (("reversa2Y","X"),(L,"reversa2Y")),
        (("reversa2Y","Y"),(L,"reversa2Y")),
        (("reversa2Y","Z"),(L,"reversa2Y")),
        (("reversa2Z","X"),(L,"reversa2Z")),
        (("reversa2Z","Y"),(L,"reversa2Z")),
        (("reversa2Z","Z"),(L,"reversa2Z")),
        (("reversa2X",blank),(L,"reversa1X")),
        (("reversa2Y",blank),(L,"reversa1Y")),
        (("reversa2Z",blank),(L,"reversa1Z")),
        (("reversa1X","X"),(L,"reversa1X")),
        (("reversa1X","Y"),(L,"reversa1X")),
        (("reversa1X","Z"),(L,"reversa1X")),
        (("reversa1Y","X"),(L,"reversa1Y")),
        (("reversa1Y","Y"),(L,"reversa1Y")),
        (("reversa1Y","Z"),(L,"reversa1Y")),
        (("reversa1Z","X"),(L,"reversa1Z")),
        (("reversa1Z","Y"),(L,"reversa1Z")),
        (("reversa1Z","Z"),(L,"reversa1Z")),
        (("reversa1X","@"),(O "X","reversa")),
        (("reversa1Y","@"),(O "Y","reversa")),
        (("reversa1Z","@"),(O "Z","reversa")),
        (("reversa","X"),(L,"reversaFinal")),
        (("reversa","Y"),(L,"reversaFinal")),
        (("reversa","Z"),(L,"reversaFinal")),
        (("reversaFinal",blank),(R,"fin1")),
        (("fin1","X"),(R,"fin1")),
        (("fin1","Y"),(R,"fin1")),
        (("fin1","Z"),(R,"fin1")),
        (("fin1",blank),(R,"fin2")),
        (("fin2","X"),(R, "fin2")),
        (("fin2","Y"),(R, "fin2")),
        (("fin2","Z"),(R, "fin2")),
        (("fin2",blank),(O blank, halt))
    ]

-- Tests
-- exec par cintaParCasoBorde
-- exec par cintaPar
-- exec par cintaImpar
-- exec shiftRight cintaShiftRightCasoBorde
-- exec shiftRight cintaShiftRight
-- exec reverse cintaReverseCasoBorde
-- exec reverse cintaReverse