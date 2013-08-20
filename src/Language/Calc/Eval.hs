module Language.Calc.Eval where

import Language.Calc.Token
import Language.Calc.Expr
import Language.Calc.Scanner
import Language.Calc.Parser
import Language.Calc.Options
import Control.Monad
import Control.Monad.State
import System.IO(hPutStrLn, hPutStr, hFlush, hIsEOF, hGetLine, stdout, stderr, stdin)
import System.Exit(exitSuccess)
import Text.Parsec

type EvalState = StateT Int IO

evalExpr :: Expr -> EvalState Int
evalExpr (EInt i) = return $ i
evalExpr ERead  = get
evalExpr (EStore expr) = do i <- evalExpr expr
                            put i
                            return $ i  
evalExpr (EAdd lexpr rexpr) =  evalExpr2 (+) lexpr rexpr
evalExpr (ESub lexpr rexpr) =  evalExpr2 (-) lexpr rexpr
evalExpr (EMul lexpr rexpr) =  evalExpr2 (*) lexpr rexpr
evalExpr (EDiv lexpr rexpr) =  evalExpr2 (div) lexpr rexpr

evalExpr2 :: (Int -> Int -> Int) -> Expr -> Expr -> EvalState Int
evalExpr2 op lexpr rexpr = do l <- evalExpr lexpr
                              r <- evalExpr rexpr
                              return $ (l `op` r) 

callEval :: Options -> Expr -> EvalState ()
callEval opts expr = do
  if optShowOutParser opts
   then  lift $ putStrLn $ show expr
   else return () 
  put 0
  r <- evalExpr expr
  lift $ hPutStrLn stdout $ show r
  
errParser :: ParseError -> EvalState ()
errParser e = lift $ hPutStrLn stderr $ show e

parseExpr :: Options -> Tokens -> EvalState ()
parseExpr opts tkns = do
  if optShowOutScan opts
   then do 
     lift $ putStrLn $ show tkns
   else return ()
  either errParser (callEval opts) (parse pExpr "Std input" tkns)


process :: Options -> EvalState ()
process opts = do
  lift $ hPutStr stdout "> "
  lift $ hFlush stdout
  l <- lift $ hGetLine stdin
  isEOF <- lift $ hIsEOF stdin
  if isEOF || null l
   then lift $ exitSuccess
   else do either errParser (parseExpr opts) (parse pTokens "Std input" l)
           process opts

runEval :: Options ->IO ()
runEval opts = evalStateT (process opts) 0
