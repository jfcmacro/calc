module Language.Calc.Parser where

import Text.Parsec.Error
import Text.Parsec.Prim
import Language.Calc.Token
import Language.Calc.Expr

type Parser = Parsec Tokens ()

pInt :: Parser Expr
pInt =

pSatisfy :: (Token -> Bool) Parser Token
pSatisfy f = tokenPrim (\c -> show [c])
                       (\pos c _ cs -> updatePos
