{-# OPTIONS_GHC -fno-warn-tabs -fno-warn-overlapping-patterns #-}

-- Nicolás Andreoli - 210630

module Turing where

--Ej1
type Symbol = String
blank :: Symbol
blank = "#"
type Tape = [Symbol]

--Ej2
data Action = Left Action | Right Action | Write Symbol Action
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
-- step::Config->Config

--Ej5
-- exec::Code ->Tape->Tape

--Ej6
-- par
-- shift-right
-- reverse