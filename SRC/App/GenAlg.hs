{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SRC.App.GenAlg ( findStablePortfolio
) where
  
import SRC.App.PoolGenerator
import SRC.App.UsefullFunc
import Data.Maybe
import Data.List (sum, map, nub, lookup)
import System.Random (mkStdGen, random, randoms)
import System.IO(IOMode(..))
import GA (Entity(..), GAConfig(..), 
           evolveVerbose, evolve, randomSearch)

instance Entity [(String, [Double])] Double [(String, [Double])] [(String, [Double])] IO where

    -- generate a random entity, i.e. a random list of quot  
    genRandom pool seed = return $ take n $ Data.List.map ((!!) pool) is
        where
          g = mkStdGen seed
          --n = 1 + (fst $ random g) `mod` 3
          n = 3
          k = length pool
          notIs = Data.List.map (flip mod k) $ randoms g
          is = Data.List.nub notIs    
 
    -- crossover operator: mix (and trim to shortest entity)
    crossover _ _ seed e1 e2 = return $ Just $ take 3 $ Data.List.nub $ e ++ e1 ++ e2
        where
          g = mkStdGen seed
          cps = zipWith (\ x y -> [x, y]) e1 e2
          picks = Data.List.map (flip mod 2) $ randoms g
          e = zipWith (!!) cps picks

    -- mutation operator: delete old and add new random quotes (max. 2)
    mutation pool p seed e = return $ Just $ take 3 $ Data.List.nub $ (drop x e) ++ addQuotes
        where
          g = mkStdGen seed
          k = round (p*4.0) :: Int
          x = mod (fst (random g)) k
          is = Data.List.map (flip mod $ length pool) $ randoms g
          addQuotes = Data.List.map ((!!) pool) is

    -- calculate the goodness of quotes
    score scoreAscList eAscList = do
      let
            e = map (\(a,b) -> b) $ eAscList ++ scoreAscList
            n = fromIntegral $ length $ e !! 0
            buyPrice = funOfSomeElement (+) e 0
            sumList = funOfListsElements (+) e
            goodness = (Data.List.sum $Data.List.map (\ x -> (x - buyPrice)**2.0) sumList) / n / buyPrice / buyPrice
        --print e
      return $ Just goodness 

    -- whether or not a scored entity is perfect
    isPerfect (_, s) = s == 0.0

findStablePortfolio :: String
                    -> String
                    -> [String]
                    -> [String]
                    -> ( Int
                       , Int
                       , Int
                       , Float
                       , Float
                       , Float
                       , Float)
                    -> IO ()
findStablePortfolio market dbName timePeriod
                    portfolioSymbolsList
                    (population, bests, maxGen, crossRate, mutRate, crossPar, mutPar) =
    do
      quotesPool <- makeTotalPool market dbName timePeriod
      let cfg = GAConfig 
                population -- 1000 -- population size
                bests      -- 50   -- archive size (best entities to keep track of)
                maxGen     -- 300  -- maximum number of generations
                crossRate  -- 0.8  -- crossover rate (% of entities by crossover)
                mutRate    -- 0.5  -- mutation rate (% of entities by mutation)
                crossPar   -- 0.0  -- parameter for crossover (not used here)
                mutPar     -- 0.2  -- parameter for mutation (% of replaced letters)
                False              -- whether or not to use checkpointing
                False              -- don't rescore archive in each generation

          g = mkStdGen 0 -- random generator
          portfolio = concat $ Data.List.map (\x -> filter (\ (a,_) -> a == x) quotesPool) portfolioSymbolsList
      putStrLn "Your portfolio is:"
      print portfolio
      -- pool of characters to pick from: printable ASCII characters
      -- Do the evolution!
      es <- evolveVerbose g cfg quotesPool portfolio
--      es <- evolve g cfg quotesPool portfolio
      let e = snd $ head es :: [(String, [Double])]
          
      putStrLn $ "best entity (GA): " ++ (show $ Data.List.map (\(a,b) -> a) e)
