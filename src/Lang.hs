module Lang where

type Symbol = Char
type Alphabet = [Symbol]

type State = String

type Transition = (State, Symbol, Symbol, Symbol, State)

data Automaton = PDA -- Pushdown automaton
  { inputAlph :: Alphabet
  , stackAlph :: Alphabet
  , states :: [State]
  , accStates :: [State]
  , transitions :: [Transition]
  }
  deriving Show