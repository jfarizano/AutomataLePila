{
module Parse where

import Lang
import Data.Char
}

%name parsePDA
%tokentype { Token }
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
symbols : var             { [checkSymbol $1]}
        | symbols ',' var { checkSymbol $3 : $1 }

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
transition : '(' var ',' var ',' var ',' var ',' var ')'  { ($2, checkSymbol $4, checkSymbol $6, checkSymbol $8, $10) }


{

parseError :: [Token] -> a
parseError [] = error $ "Parse error: empty file"
parseError xs = error $ "Parse error " ++ (show $ head xs)

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
           deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer ('=' : cs) = TEq : lexer cs
lexer (',' : cs) = TComma : lexer cs
lexer (';' : cs) = TSemicolon : lexer cs
lexer ('(' : cs) = TLParen : lexer cs 
lexer (')' : cs) = TRParen : lexer cs
lexer ('{' : cs) = TLBrace : lexer cs
lexer ('}' : cs) = TRBrace : lexer cs
lexer x@(c:cs) = if isSpace c then lexer cs
                              else lexvar x

lexvar cs = case span isAcceptedSymbol cs of
                ("InputAlph", rest) -> TInputAlph : lexer rest
                ("StackAlph", rest) -> TStackAlph : lexer rest
                ("States", rest) -> TStates : lexer rest
                ("AccStates", rest) -> TAccStates : lexer rest
                ("Transitions", rest) -> TTransitions : lexer rest
                (var, rest) -> TVar var : lexer rest

reservedSymbols = "=,;(){}"

isAcceptedSymbol = \c -> (isPrint c) && (not $ (c `elem` reservedSymbols) || (isSpace c) || (isControl c))

checkSymbol :: String -> Char
checkSymbol xs = if length xs == 1 then head xs else error $ "Invalid symbol " ++ xs ++ ". Give only one char at a time."
}
