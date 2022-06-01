{-# OPTIONS_GHC -fno-warn-tabs -fno-warn-overlapping-patterns #-}

-- Nicolás Andreoli - 210630

module Turing where

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
step c (q,t@(l:ls,x,r:rs)) = case q == halt of {
    True -> (q,t);
    False -> case a of {
        R -> (s,(ls,l,x:r:rs));
        L -> (s,(x:l:ls,r,rs));
        O z -> (s,(l:ls,z,r:rs));
    } where {
    (a,s) = lookupTuring q x c;
    }
}


--Ej5
-- exec :: Code -> Tape -> Tape
-- exec c t = 


--Ej6
-- par
-- shift-right
-- reverse