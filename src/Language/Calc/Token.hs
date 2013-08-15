module Language.Calc.Token(Tokens
                          ,Token(..)
                          ,isTknInt) where

type Tokens = [Token]

data Token = TknInt Int
           | TknSMem
           | TknRMem
           | TknOp Char
           | TknEOF
           | TknOPar
           | TknCPar
             deriving (Show)

isTknInt :: Token -> Bool
isTknInt (TknInt _) = True
isTknInt _          = False
