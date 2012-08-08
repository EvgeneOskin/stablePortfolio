module SRC.App.MyIO ( processArgs
                    , run
                    , Options(..)
) where

import SRC.App.GenAlg
import System.Environment (getArgs, getEnv)
import System.Console.GetOpt

processArgs :: [String] -> IO (Options, [String])
processArgs argv =
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
   where header = "Usage: csvLoader [OPTION...]"

data Options = Options
 { optMarket      :: String
 , optInputDir    :: String
 , optTimePeriod  :: String
 , optHelp        :: Bool
 } deriving Show

defaultOptions :: Options
defaultOptions    = Options
 { optMarket      = ""
 , optInputDir    = ""
 , optTimePeriod  = ""
 , optHelp        = False
 }

options :: [OptDescr (Options -> Options)]
options =
 [ Option ['M'] ["market"]
              (ReqArg (\ f opts -> opts { optMarket     = f })
                          "STR") "market name"
 , Option ['I'] ["input_dir"]
              (ReqArg (\ f opts -> opts { optInputDir   = f })
                         "STR") "output directory"
 , Option ['T'] ["output_dir"]
              (ReqArg (\ f opts -> opts { optTimePeriod = f })
                         "DATE") "output directory"
 , Option ['H'] ["help"]
              (NoArg (\   opts -> opts { optHelp        = True })
              ) "help"
 ]

run (Options _ _ _ True, _)  = do
  putStrLn $ usageInfo "" options

run (Options market inDN timePeriod _, _)  = do
  findStablePortfolio market inDN timePeriod

