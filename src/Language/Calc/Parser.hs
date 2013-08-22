module Language.Calc.Parser where

import Text.Parsec.Error
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Language.Calc.Token
import Language.Calc.Expr
import Language.Calc.Scanner
import Control.Applicative((<$),(<$>),(<**>),(<*))

type Parser = Parsec Tokens ()

pProg :: Parser Expr
pProg = pExpr <* pEOF

pExpr :: Parser Expr
pExpr = pTerm `chainl1` pOperSum

pTerm :: Parser Expr
pTerm = pStorable `chainl1` pOperMult

pOperSum = pOper charOperSum

pOperMult = pOper charOperMult

pStorable :: Parser Expr
pStorable = pFact <**> option id pStore

pFact :: Parser Expr
pFact =  EInt <$> pInt
         <|>
         ERead <$ pRead
         <|>
         do e <- between pOpenPar pClosePar pExpr
            option e (EStore e <$ pStore)

calcToken :: (Tok -> Maybe a) -> Parser a
calcToken test = token showToken posToken testToken
   where showToken (pos,tok) = show tok
         posToken  (pos,tok) = pos
         testToken (pos,tok) = test tok

pEOF :: Parser ()
pEOF = calcToken (\tok -> case tok of
                             TknEOF -> Just ()
                             other  -> Nothing)
       
pInt :: Parser Int
pInt = calcToken (\tok -> case tok of
                             TknInt i -> Just i
                             other    -> Nothing)

pStore :: Parser (Expr -> Expr)
pStore = calcToken (\tok -> case tok of 
                              TknSMem -> Just $ EStore
                              other   -> Nothing)

pRead :: Parser ()
pRead = calcToken (\tok -> case tok of
                              TknRMem -> Just ()
                              other  -> Nothing)

pOpenPar :: Parser ()
pOpenPar = calcToken (\tok -> case tok of
                                TknOPar -> Just ()
                                other   -> Nothing)

pClosePar :: Parser ()
pClosePar = calcToken (\tok -> case tok of
                                 TknCPar -> Just ()
                                 other   -> Nothing)

pOper :: (Char -> (Maybe (Expr -> Expr -> Expr))) 
      -> Parser (Expr -> Expr -> Expr)
pOper f = calcToken (\tok -> case tok of 
                                TknOp c -> f c
                                other  -> Nothing)

charOperSum :: Char -> Maybe (Expr -> Expr -> Expr)
charOperSum '+' = Just EAdd
charOperSum '-' = Just ESub
charOperSum _   = Nothing

charOperMult :: Char -> Maybe (Expr -> Expr -> Expr)
charOperMult '*' = Just EMul
charOperMult '/' = Just EDiv
charOperMult _   = Nothing
