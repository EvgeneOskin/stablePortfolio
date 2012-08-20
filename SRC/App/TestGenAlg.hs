module SRC.App.TestGenAlg ( prop_findStablePortfolio
                          , myTest
) where

import SRC.App.PoolGenerator
import Test.QuickCheck
import SRC.App.UsefullFunc
import SRC.App.GenAlg
import Data.List
import Control.Monad.Trans
import System.Random

prop_findStablePortfolio :: [(String, [Double])] -> Int -> Double -> Int -> Bool
prop_findStablePortfolio pool nEntity bestGoodness a = bestGoodness < (calculateGoodness
                                                          $ getRandomEntity pool nEntity a)

getRandomEntity pool nEntity seed = map ((!!) pool) indexs
    where g = mkStdGen $ seed*seed
          n = length pool
          indexs = take nEntity $ Data.List.nub $ Data.List.map (`mod` n) $ randoms g

                         
defaultParametersGA :: (Int, Int, Int, Float, Float, Float, Float)
defaultParametersGA = (2000, 500, 100, 0.8, 0.5, 0.0, 0.8) 
                         
myTest parametersGA dbName timePeriod = do
  putStrLn ">>>Start Test"
  quotesPool <- makeTotalPool "market" dbName timePeriod
  bestEntity <- findStablePortfolio quotesPool [""] parametersGA
  let argsStablePortfolio = (Args Nothing 10000 100 10000 True)
      nEntity = length bestEntity
      bestGoodness = calculateGoodness bestEntity
  result <- quickCheckWithResult argsStablePortfolio (prop_findStablePortfolio quotesPool nEntity bestGoodness)
  return result