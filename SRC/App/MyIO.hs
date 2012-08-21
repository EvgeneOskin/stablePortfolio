module SRC.App.MyIO ( processArgs
                    , run
                    , Options(..)
) where

import Test.QuickCheck (Result(..))
import SRC.App.PoolGenerator
import SRC.App.TestGenAlg
import SRC.App.GenAlg
import System.Environment (getArgs, getEnv)
import System.Console.GetOpt
import Data.List.Split (endBy)

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
 , optPopulation  :: String
 , optArchiveSize :: String
 , optMaxGensNum  :: String
 , optCrossRate   :: String
 , optMutatRate   :: String
 , optCrossPar    :: String
 , optMutatPar    :: String
 , optPortfolio   :: String
 , optCapital     :: String
 , optTest        :: Bool
 , optHelp        :: Bool
 } deriving Show

defaultOptions :: Options
defaultOptions    = Options
 { optMarket      = ""
 , optInputDir    = ""
 , optTimePeriod  = ""
 , optPopulation  = "1000"
 , optArchiveSize = "100"
 , optMaxGensNum  = "150"
 , optCrossRate   = "0.8"
 , optMutatRate   = "0.2"
 , optCrossPar    = "0.0"
 , optMutatPar    = "0.5"
 , optPortfolio   = ""
 , optCapital     = "0.0"
 , optTest        = False
 , optHelp        = False
 }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['M'] ["market"]
                 (ReqArg (\ f opts -> opts { optMarket      = f })
                             "\"STR\"") "market name"
    , Option ['I'] ["input_db"]
                 (ReqArg (\ f opts -> opts { optInputDir    = f })
                         "\"STR\"") "database with quote"
    , Option ['D'] ["date_period"]
                 (ReqArg (\ f opts -> opts { optTimePeriod  = f })
                             "\"DATE DATE\"") "period of date"
    , Option ['P'] ["population"]
                 (ReqArg (\ f opts -> opts { optPopulation  = f })
                             "\"INT\"") "population size\n defualt 1000"
    , Option ['A'] ["archive"]
                 (ReqArg (\ f opts -> opts { optArchiveSize = f })
                             "\"INT\"")
                 "archive size (best entities to keep track of)\n defualt 200"
    , Option ['G'] ["max_gens"]
                 (ReqArg (\ f opts -> opts { optMaxGensNum  = f })
                             "\"INT\"")
                 "maximum number of generations\n defualt 300"
    , Option ['x'] ["cross_rate"]
                 (ReqArg (\ f opts -> opts { optCrossRate   = f })
                             "\"FLOAT\"")
                 "crossover rate (% of entities by crossover)\n defualt 0.8"
    , Option ['y'] ["mutat_rate"]
                 (ReqArg (\ f opts -> opts { optMutatRate   = f })
                             "\"FLOAT\"")
                 "mutation rate (% of entities by mutation)\n defualt 0.2"
    , Option ['X'] ["Cross_Par"]
                 (ReqArg (\ f opts -> opts { optCrossPar    = f })
                             "\"FLOAT\"")
                 "parameter for crossover (not used here)\n defualt 0.0"
    , Option ['Y'] ["Mutat_Par"]
                 (ReqArg (\ f opts -> opts { optMutatPar    = f })
                             "\"FLOAT\"")
                 "parameter for mutation (% of replaced quotes)\n defualt 0.5"
    , Option ['S'] ["portfolio"]
                 (ReqArg (\ f opts -> opts { optPortfolio    = f })
                             "\"STR\"") "List of Symbols which you have\n defualt \"\""
    , Option ['C'] ["capital"]
                 (ReqArg (\ f opts -> opts { optCapital      = f })
                             "\"DOUBLE\"") "What Capital do you have\n defualt \"0.0\""
    , Option ['T'] ["test"]
                 (NoArg (\    opts -> opts { optTest         = True })
                 ) "run only after success test"
    , Option ['H'] ["help"]
                 (NoArg (\    opts -> opts { optHelp         = True })
                 ) "help"
    ]

run :: (Options, t) -> IO ()  
run (Options _ _ _ _ _ _ _ _ _ _ _ _ _ True, _)  = do
  putStrLn $ usageInfo "" options

run (Options market dbName timePeriodStr
             population bests maxGen crossRate mutRate
                        crossPar mutPar _ _ True _, _)  = do
  let parametersGA = ( read population :: Int -- 1000 -- population size
                     , read bests      :: Int -- 50   -- archive size (best entities to keep track of)
                     , read maxGen     :: Int  -- 300  -- maximum number of generations
                     , read crossRate  :: Float -- 0.8  -- crossover rate (% of entities by crossover)
                     , read mutRate    :: Float  -- 0.5  -- mutation rate (% of entities by mutation)
                     , read crossPar   :: Float -- 0.0  -- parameter for crossover (not used here)
                     , read mutPar     :: Float -- 0.2  -- parameter for mutation (% of replaced letters)
                     )
      timePeriod = (endBy " " timePeriodStr)
  result <- myTest parametersGA dbName timePeriod
  print result
  case result of
    (Success _ _ _) -> do
                putStrLn "All OK. You can work with this parameters"
    _               -> do
                putStrLn "This is not good parameters:"
                putStrLn "    try to increase population or archive size (\"-P\" and \"-A\" flags) "
  putStrLn $ "Parameters was: " ++ show parametersGA 

           
run (Options market dbName timePeriodStr
             population bests maxGen crossRate mutRate
                        crossPar mutPar portfolioStrCsv capitalStr _ _, _)  = do
  let parametersGA = ( read population :: Int -- 1000 -- population size
                     , read bests      :: Int -- 100   -- archive size (best entities to keep track of)
                     , read maxGen     :: Int  -- 150  -- maximum number of generations
                     , read crossRate  :: Float -- 0.8  -- crossover rate (% of entities by crossover)
                     , read mutRate    :: Float  -- 0.5  -- mutation rate (% of entities by mutation)
                     , read crossPar   :: Float -- 0.0  -- parameter for crossover (not used here)
                     , read mutPar     :: Float -- 0.2  -- parameter for mutation (% of replaced letters)
                     )
      capital = read capitalStr :: Double
      timePeriod = (endBy " " timePeriodStr)
      portfolio = endBy "," portfolioStrCsv
  quotesPool <- makeTotalPool market dbName timePeriod
  bestEntity <- findStablePortfolio quotesPool (capital, portfolio) parametersGA
  return ()
