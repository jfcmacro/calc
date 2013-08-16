module Language.Calc.Parser where

import Text.Parsec.Error
import Text.Parsec.Prim
import Language.Calc.Token
import Language.Calc.Expr

type Parser = Parsec Tokens ()

