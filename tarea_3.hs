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
type Code = [(State,(Symbol,Action,State))]

--Ej3
type Config = (State,Tape)

--Ej4
err :: String
err = "error en tiempo de ejecución"

step :: Code -> Config -> Config
step c (q,t) = case lookup q c of {
    Nothing -> error err;
    Just (x,a,s) -> case a of {
        L -> error err;
        R -> error err;
        O x -> error err;
    };
}

--Ej5
-- exec :: Code -> Tape -> Tape
-- exec [] t = t
-- exec c@((si,a,a,sf):cs) t = step c (si,t)


--Ej6
-- par
-- shift-right
-- reverse