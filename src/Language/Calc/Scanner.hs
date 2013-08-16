module Language.Calc.Scanner where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Pos(SourcePos)
import Language.Calc.Token(Tokens, Token, Tok(..))
import Control.Monad.State(get)


getPos :: Parser SourcePos
getPos = do s <- getParserState
            return $ statePos s

pTokens :: Parser Tokens
pTokens = do l <- pToken
             ini <- getPos
             return $ l++[(ini, TknEOF)]

pToken :: Parser [Token]
pToken =  sepBy  (choice [pTknInt, pTknOp, pTknMem, pTknOPar, pTknCPar]) spaces
                 
pTknInt :: Parser Token
pTknInt = do ini <- getPos 
             i  <- oneOf "123456789" 
             is <- many digit
             return $ (ini, TknInt (read (i:is)))

pTknOp :: Parser Token
pTknOp = do ini <- getPos 
            o <- oneOf "+*-/"
            return $ (ini, TknOp o)

pTknMem :: Parser Token
pTknMem = do ini <- getPos 
             o <- oneOf "S"
             -- spaces
             return $ (ini, TknSMem)
          <|>
         do ini <- getPos 
            o <- oneOf "R"
            -- spaces
            return $ (ini, TknRMem)

pTknOPar :: Parser Token
pTknOPar = do ini <- getPos 
              o <- oneOf "("
              -- spaces
              return $ (ini, TknOPar)

pTknCPar :: Parser Token
pTknCPar = do ini <- getPos 
              o <- oneOf ")"
              -- spaces
              return $ (ini, TknCPar)

pTknEOF :: Parser Token
pTknEOF = do ini <- getPos 
             eof
             return $ (ini, TknEOF)


