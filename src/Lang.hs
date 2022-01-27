module Lang where

type Symbol = Char
type Alphabet = [Symbol]

type State = String

type Transition = (State, Symbol, Symbol, Symbol, State)

data Automata = PDA -- Pushdown automaton
  { inputAlph :: Alphabet
  , stackAlph :: Alphabet
  , states :: [State]
  , accStates :: [State]
  , initState :: State
  , transitions :: [Transition]
  }
  deriving Show