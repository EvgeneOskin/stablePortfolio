{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SRC.App.GenAlg ( findStablePortfolio
) where
  
import SRC.App.PoolGenerator
import SRC.App.UsefullFunc
import Data.Maybe
import Data.List (sum, map)
import System.Random (mkStdGen, random, randoms)
import System.IO(IOMode(..))
import GA (Entity(..), GAConfig(..), 
           evolveVerbose, randomSearch)

instance Entity [(String, [Double])] Double Int [(String, [Double])] IO where

    -- generate a random entity, i.e. a random list of quot  
    genRandom pool seed = return $ take n $ Prelude.map ((!!) pool) is
        where
          g = mkStdGen seed
          n = (fst $ random g) `mod` 4
          k = length pool
          is = Prelude.map (flip mod k) $ randoms g
 
    -- crossover operator: mix (and trim to shortest entity)
    crossover _ _ seed e1 e2 = return $ Just e
        where
          g = mkStdGen seed
          cps = zipWith (\x y -> [x, y]) e1 e2
          picks = Prelude.map (flip mod 2) $ randoms g
          e = zipWith (!!) cps picks

    -- mutation operator: delete old and add new random quotes (max. 2)
    mutation pool p seed e = return $ Just $ (drop x e) ++ addQuotes
        where
          g = mkStdGen seed
          k = round (p*4.0) :: Int
          x = mod (fst (random g)) k
          is = Prelude.map (flip mod $ length pool) $ randoms g
          addQuotes = take x $ Prelude.map ((!!) pool) is

    -- calculate the goodness of quotes
    score' _ eAscList = Just goodness 
        where
          e = map (\(a,b) -> b) eAscList
        -- e is List of Lists
          n = fromIntegral $ length $ e !! 0
          buyPrice = funOfSomeElement (+) e 0
          sumList = funOfListsElements (+) e
          goodness = (Data.List.sum $Data.List.map (\ x -> (x - buyPrice)**2.0) sumList) / n

    -- whether or not a scored entity is perfect
    isPerfect (_, s) = s == 0.0

findStablePortfolio :: String -> String -> (String, String) -> IO ()
findStablePortfolio market inDN timePeriod =
    do
      let cfg = GAConfig 
                100 -- population size
                25 -- archive size (best entities to keep track of)
                300 -- maximum number of generations
                0.8 -- crossover rate (% of entities by crossover)
                0.5 -- mutation rate (% of entities by mutation)
                0.0 -- parameter for crossover (not used here)
                0.75 -- parameter for mutation (% of replaced letters)
                False -- whether or not to use checkpointing
                False -- don't rescore archive in each generation

          g = mkStdGen 0 -- random generator
                       
      -- pool of characters to pick from: printable ASCII characters
      quotesPool <- makeTotalPool market inDN timePeriod
      -- Do the evolution!
      es <- evolveVerbose g cfg quotesPool 1
      let e = snd $ head es
          
      putStrLn $ "best entity (GA): " ++ (show $ map (\(a,b) -> a) e)
