{-# LANGUAGE DeriveDataTypeable #-}

module Lang where

import System.Console.CmdArgs.Implicit

-- | Tipos que representan las partes de un autómata y su configuración.
type Symbol = Char
type Alphabet = [Symbol]

type State = String

type Stack = [Symbol]

-- | Tupla que representa una configuración de la máquina, compuesta por:
-- (Estado actual, palabra restante a leer, contenido actual de la pila)
type Config = (State, String, Stack)

-- | Tupla que representa una transición del autómata, compuesta por:
-- (Estado actual, simbolo a leer de la palabra, simbolo a sacar de la pila, 
-- simbolo pongo en pila, estado siguiente)
type Transition = (State, Symbol, Symbol, Symbol, State)

-- | Record que representa a un autómata de pila
data Automaton = PDA { -- Pushdown automaton
    inputAlph :: Alphabet, -- Alfabeto de entrada
    stackAlph :: Alphabet, -- Alfabeto de pila
    states :: [State], -- Estados
    accStates :: [State], -- Estados de aceptación
    transitions :: [Transition] -- Transiciones
} deriving (Show, Data, Typeable)