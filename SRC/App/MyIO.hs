module SRC.App.MyIO ( processArgs
                    , run
                    , Options(..)
) where

import SRC.App.GenAlg
import System.Environment (getArgs, getEnv)
import System.Console.GetOpt
import Data.List.Split

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
                          "\"STR\"") "market name"
 , Option ['I'] ["input_dir"]
              (ReqArg (\ f opts -> opts { optInputDir   = f })
                         "\"STR\"") "output directory"
 , Option ['T'] ["time_period"]
              (ReqArg (\ f opts -> opts { optTimePeriod = f })
                         "\"DATE DATE\"") "period of date"
 , Option ['H'] ["help"]
              (NoArg (\   opts -> opts { optHelp        = True })
              ) "help"
 ]

run :: (Options, t) -> IO ()
run (Options _ _ _ True, _)  = do
  putStrLn $ usageInfo "" options

run (Options market inDN timePeriodStr _, _)  = do
  let parametersGA = ( 1000 -- population size
                     , 50 -- archive size (best entities to keep track of)
                     , 300 -- maximum number of generations
                     , 0.8 -- crossover rate (% of entities by crossover)
                     , 0.2 -- mutation rate (% of entities by mutation)
                     , 0.0 -- parameter for crossover (not used here)
                     , 0.5 -- parameter for mutation (% of replaced letters)
                    )
      timePeriod = (endBy " " timePeriodStr)
  findStablePortfolio market inDN timePeriod parametersGA
