{-# LANGUAGE DeriveDataTypeable #-}

module Lang where

import System.Console.CmdArgs.Implicit

type Symbol = Char
type Alphabet = [Symbol]

type State = String

type Stack = [Symbol]

type Step = (State, String, Stack)

-- (Estado actual, simbolo a leer palabra, simbolo a sacar de la pila, simbolo pongo en pila, estado siguiente)
type Transition = (State, Symbol, Symbol, Symbol, State)

data Automaton = PDA { -- Pushdown automaton
    inputAlph :: Alphabet,
    stackAlph :: Alphabet,
    states :: [State],
    accStates :: [State],
    transitions :: [Transition]
} deriving (Show, Data, Typeable)