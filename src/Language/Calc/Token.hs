module Language.Calc.Token(Tokens
                          ,Token, Tok(..)) where

import Text.Parsec.Pos(SourcePos)

type Tokens = [Token]

type Token = (SourcePos, Tok)

data Tok = TknInt Int
         | TknSMem
         | TknRMem
         | TknOp Char
         | TknEOF
         | TknOPar
         | TknCPar
         deriving (Show)

