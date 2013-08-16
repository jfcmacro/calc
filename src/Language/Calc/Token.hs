module Language.Calc.Token(Tokens
                          ,Token(..)) where

import Text.Parsec.Pos(SourcePos)

type Tokens = [Token]

data Token = TknInt Int SourcePos
           | TknSMem SourcePos
           | TknRMem SourcePos
           | TknOp Char SourcePos
           | TknEOF SourcePos
           | TknOPar SourcePos
           | TknCPar SourcePos
             deriving (Show)

