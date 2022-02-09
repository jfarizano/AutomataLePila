{
module Parse (
    parsePDA
) where

import Data.Char ( isPrint, isSpace, isControl )

import Lang
}

%name parse_PDA
%tokentype { Token }
%monad { P } { thenP } { returnP }
%lexer { monadicLexer } { TEOF }
%error { parseError }

%token
    InputAlph         { TInputAlph }
    StackAlph         { TStackAlph }
    States            { TStates }
    AccStates         { TAccStates }
    Transitions       { TTransitions }
    '='               { TEq }
    ','               { TComma }
    ';'               { TSemicolon }
    '('               { TLParen }
    ')'               { TRParen }
    '{'               { TLBrace }
    '}'               { TRBrace }
    var               { TVar $$ }

%%

PDA :: { Automaton }
PDA : InputAlph   '=' alph ';'
      StackAlph   '=' alph ';'
      States      '=' states  ';'   
      AccStates   '=' states  ';'
      Transitions '=' transitions { PDA (reverse $3) (reverse $7) (reverse $11) (reverse $15) (reverse $19) }

alph :: { Alphabet }
alph : '{' symbols '}'  { $2 }
     | '{' '}'          { [] }

symbols :: { Alphabet }
symbols : var             { % checkSymbol $1 `thenP` (\c -> returnP [c]) }
        | symbols ',' var { % checkSymbol $3 `thenP` (\c -> returnP (c : $1)) }

states :: { [State] }
states : '{' sts '}'  { $2 }
       | '{' '}'      { [] }

sts :: { [State] }
sts : var         { [$1] }
    | sts ',' var { $3 : $1 }

transitions :: { [Transition] }
transitions : '{' trns '}'  { $2 }
            | '{' '}'       { [] }

trns :: { [Transition] }
trns : transition          { [$1] }
     | trns ',' transition { $3 : $1 }

transition :: { Transition }
transition : '(' var ',' var ',' var ',' var ',' var ')'  {% (buildTransition $2 $4 $6 $8 $10) }

{

parsePDA :: String -> Either String Automaton
parsePDA s = parse_PDA s 0

type P a = String -> Int -> Either String a

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s i -> case m s i of
                       Left e -> Left e
                       Right a -> k a s i

getLineNum :: P Int
getLineNum = \s l -> Right l

returnP :: a -> P a
returnP a = \_ _ -> Right a

failP :: String -> P a
failP e = \_ _ -> Left e

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s i -> case m s i of
                       Left e -> k e s i
                       Right a -> Right a

parseError :: Token -> P a
parseError t = getLineNum `thenP` \line -> 
               failP ("Error de parseo en linea: " ++ show line ++ ". Token: " ++ show t)

data Token = TInputAlph 
           | TStackAlph 
           | TStates 
           | TAccStates 
           | TTransitions 
           | TEq 
           | TComma
           | TSemicolon 
           | TLParen 
           | TRParen 
           | TLBrace 
           | TRBrace 
           | TVar String
           | TEOF
           | TCharError Char
           deriving Show

monadicLexer :: (Token -> P a) -> P a
monadicLexer cont str =
    case str of
        [] -> cont TEOF str
        '\n':str' -> \n -> monadicLexer cont str' (n + 1)
        _ -> let (token, str') = lexer str in cont token str'

lexer :: String -> (Token, String)
lexer [] = (TEOF, [])
lexer ('=' : cs) = (TEq, cs)
lexer (',' : cs) = (TComma, cs)
lexer (';' : cs) = (TSemicolon, cs)
lexer ('(' : cs) = (TLParen, cs) 
lexer (')' : cs) = (TRParen, cs)
lexer ('{' : cs) = (TLBrace, cs)
lexer ('}' : cs) = (TRBrace, cs)
lexer x@(c:cs) | isSpace c = lexer cs
               | isAcceptedSymbol c = lexvar x
               | otherwise = (TCharError c, cs)

lexvar :: String -> (Token, String)
lexvar cs = case span isAcceptedSymbol cs of
                ("InputAlph", rest) -> (TInputAlph, rest)
                ("StackAlph", rest) -> (TStackAlph, rest)
                ("States", rest) -> (TStates, rest)
                ("AccStates", rest) -> (TAccStates, rest)
                ("Transitions", rest) -> (TTransitions, rest)
                (var, rest) -> (TVar var, rest)

reservedSymbols = "=,;(){}"

isAcceptedSymbol = \c -> (isPrint c) && (not $ (c `elem` reservedSymbols) || (isSpace c) || (isControl c))

buildTransition :: State -> String -> String -> String -> State -> P Transition
buildTransition st0 sy0 sy1 sy2 st1 = checkSymbol sy0 `thenP` (\sy0' -> checkSymbol sy1 `thenP`
                                                              (\sy1' -> checkSymbol sy2 `thenP`
                                                              (\sy2' -> returnP (st0, sy0', sy1', sy2', st1))))

checkSymbol :: String -> P Char
checkSymbol xs = getLineNum `thenP` \line -> 
                 if length xs == 1 
                 then returnP $ head xs 
                 else failP $ "Error de parseo en línea " ++ show line ++ " al leer el símbolo " ++ xs ++ ". Dar solo un caracter a la vez."

}