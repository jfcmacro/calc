module Main where

import Language.Calc.Parser
import Language.Calc.Scanner
import Language.Calc.Token
import Language.Calc.Expr
import Language.Calc.Eval
import System.Environment(getArgs)
import Data.Version(Version(..), showVersion)
import System.Console.GetOpt
import System.IO(hPutStrLn, stdout, stderr)
import Paths_calc(version)

data Options =  Options { optShowVersion   :: Bool
                        , optShowOutScan   :: Bool
                        , optShowOutParser :: Bool
                        , optShowHelp      :: Bool
                        } deriving Show

defaultOptions :: Options
defaultOptions =  Options { optShowVersion   = False
                          , optShowOutScan   = False
                          , optShowOutParser = False
                          , optShowHelp      = False
                          }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['v'] ["version"] 
    (NoArg (\opts -> opts { optShowVersion  = True })) 
    "show version number"
  , Option ['s'] ["scanner"] 
    (NoArg (\opts -> opts { optShowOutScan = True }))
    "show scanner output"
  , Option ['p'] ["parser"] 
    (NoArg (\opts -> opts { optShowOutParser = True }))
    "show parser output"
  , Option ['h','?'] ["help"] 
    (NoArg (\opts -> opts { optShowHelp = True }))
    "show this output"
  ]

helpHeader :: String
helpHeader = "Usage: calc [OPTION..]"

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o,n,[]) -> return (foldl (flip id) defaultOptions o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = helpHeader


main :: IO ()
main = do
  (opts, fls) <- (getArgs >>= compilerOpts)
  if (optShowHelp opts)
   then hPutStrLn stdout $ usageInfo helpHeader options
   else runEval

  
