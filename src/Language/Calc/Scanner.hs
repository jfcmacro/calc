module Language.Calc.Scanner where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Pos(SourcePos)
import Language.Calc.Token(Tokens, Token(..))

pTokens :: Parser [Token]
pTokens = do l <- pToken
             return $ l++[TknEOF]

pToken :: Parser [Token]
pToken =  sepBy  (choice [pTknInt, pTknOp, pTknMem, pTknOPar, pTknCPar]) spaces
                 
pTknInt :: Parser Token
pTknInt = do i  <- oneOf "123456789"
             pos <- 
             is <- many digit
             -- spaces
             return $ TknInt (read (i:is))

pTknOp :: Parser Token
pTknOp = do o <- oneOf "+*-/"
            -- spaces
            return $ TknOp o

pTknMem :: Parser Token
pTknMem = do o <- oneOf "S"
             -- spaces
             return $ TknSMem
          <|>
         do o <- oneOf "R"
            -- spaces
            return $ TknRMem

pTknOPar :: Parser Token
pTknOPar = do o <- oneOf "("
              -- spaces
              return $ TknOPar

pTknCPar :: Parser Token
pTknCPar = do o <- oneOf ")"
              -- spaces
              return $ TknCPar

pTknEOF :: Parser Token
pTknEOF = eof >> (return $ TknEOF)


