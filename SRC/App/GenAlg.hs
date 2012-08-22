{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SRC.App.GenAlg ( findStablePortfolio
) where

import SRC.App.UsefullFunc
import GHC.Float (int2Float, int2Double)
import Data.Maybe (fromJust)
import Data.List (sum, map, nub, lookup, sort)
import System.Random (mkStdGen, randomR, randomRs)
import System.IO(IOMode(..))
import GA (Entity(..), GAConfig(..), 
           evolveVerbose, randomSearch)

instance Entity [(String, [Double])] Double (Double, [(String, [Double])]) [(String, [Double])] IO where
    -- generate a random entity, i.e. a random list of quot  
    genRandom pool seed = return $ take n $ Data.List.map ((!!) pool) is
        where
          g = mkStdGen seed
          n = fst $ randomR (1, pL) g :: Int
          pL = length pool
          notIs = randomRs (0, pL - 1) g
          is = Data.List.nub notIs    
 
    -- crossover operator: mix (and trim to shortest entity)
    crossover _ _ seed e1 e2 = return $ Just $ take n $ Data.List.nub $ e ++ e1 ++ e2
        where
          n = round $ int2Double (length e1 + length e2) / 2 :: Int
          g = mkStdGen seed
          cps = zipWith (\ x y -> [x, y]) e1 e2
          picks = randomRs (0, 1) g
          e = zipWith (!!) cps picks


    -- mutation operator: delete old and add new random quotes
    mutation pool p seed e = return $ Just $ take pick $ Data.List.nub $ dropedEntity ++ addQuotes
        where
          g = mkStdGen seed
          pL =length pool
          eL = length e
          dL = round (p*int2Float eL) :: Int
          x = fst $ randomR (0, dL) g :: Int --how much should be droped (1,dL)<-best
          dropedEntity = drop x e
          
          smallest = if eL - x < 2   then 2  else eL - x
          largest  = if eL + x >= pL then pL else eL + x
          pick =  fst $ randomR (smallest, largest) g

          newPool = filter (`notElem` dropedEntity) pool
          is = randomRs (0, pL - eL + x - 1) g
          addQuotes = Data.List.map ((!!) newPool) is
                         
    -- calculate the goodness of quotes
    score' (capital, scoreAscList) eAscList = Just $ goodnessE + goodnessP
        where tEntiy = eAscList ++ scoreAscList
              goodnessE = calculateGoodness $ tEntiy
              buyPrice = funOfSomeElement (+) (Data.List.map snd tEntiy) 0
              goodnessP = if capital /= 0.0
                            then (capital - buyPrice)^2 / capital / capital
                            else 0.0

    -- whether or not a scored entity is perfect
    isPerfect (_, s) = s == 0.0

    showGeneration index (_,s) = "(gen: " ++ show index ++ ") Best Entity " ++
                                 show (sort symbols) ++ " its length is "++ show (length symbols) ++
                                 " [fitness: " ++ show (fromJust fitness) ++ "] " ++"(gen: " ++ show index ++ ")"
        where (fitness, e) = head s
              symbols = Data.List.map fst  e

--    hasConverged xs = amountEntityUnique / amountEntity < 0.01
--        where amountEntityUnique = int2Double $ length (Data.List.nub xs)
--              amountEntity = int2Double $ length (xs)
                                   
findStablePortfolio :: [(String, [Double])]
                    -> (Double, [String])
                    -> ( Int
                       , Int
                       , Int
                       , Float
                       , Float
                       , Float
                       , Float)
                    -> IO [(String, [Double])]
findStablePortfolio quotesPool
                    (capital, portfolioSymbolsList)
                    (population, bests, maxGen, crossRate, mutRate, crossPar, mutPar) =
    do
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
          portfolio = concat $ Data.List.map (\x -> filter (\(a,_) -> a == x) quotesPool) portfolioSymbolsList
      putStrLn ("Your portfolio is: " ++ show portfolio)
      -- pool of characters to pick from: printable ASCII characters
      -- Do the evolution!
      es <- evolveVerbose g cfg quotesPool (capital, portfolio)
--      es <- evolve g cfg quotesPool portfolio
      let e = snd $ head es :: [(String, [Double])]
          
      putStrLn $ "best entity (GA): " ++ (show e) ++ "\n   with goodness "
                   ++ show (calculateGoodness e)
      return e
               