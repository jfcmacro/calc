module Language.Calc.Options where

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
