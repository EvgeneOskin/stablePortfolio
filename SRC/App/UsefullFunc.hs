module SRC.App.UsefullFunc ( changeDateFormat
                           , getDataFromStr
                           , pickDates
                           , concatData
                           , funOfListsElements
                           , funOfSomeElement
                           , separateByDate
                           , calculateGoodness
) where

  
import Data.Time (UTCTime(..))
import Data.Time.Format (readTime, formatTime)
import System.Locale (defaultTimeLocale)
import Data.List.Split (endBy)
import Data.Map (map, intersectionWith, Map(..), fromList)
import Data.List (map, filter, zipWith, nub, sum)

changeDateFormat :: String -> String -> String -> String
changeDateFormat x haveFx needFx = Data.Time.Format.formatTime System.Locale.defaultTimeLocale needFx t
    where t = Data.Time.Format.readTime System.Locale.defaultTimeLocale haveFx x :: Data.Time.UTCTime

getDataFromStr :: String -> (String, Double)
getDataFromStr str = (symbol, price)
    where splitedStr = endBy "," str
          symbol = head splitedStr
          prices = Data.List.map read (init (drop 2 splitedStr)) :: [Double]
          price = last prices
                   
pickDates :: String -> String -> [String] -> Bool
pickDates x market [low, hi] = (strForCompare >= low) && (strForCompare <= hi)
    where strForCompare = changeDateFormat (last $ endBy "/" x)
                          (market ++ "_%Y%m%d.csv") "%Y%m%d"

concatData :: Ord k => [[(k, a)]] -> Data.Map.Map k [a]
concatData (x:[]) = Data.Map.map (\a -> [a]) $ fromList x
concatData (x:xs) = Data.Map.intersectionWith (\a b -> [a] ++ b) (fromList x) $ concatData xs

funOfListsElements :: (b -> b -> b) -> [[b]] -> [b] 
funOfListsElements f (x:[]) = x
funOfListsElements f (x:xs) = Data.List.zipWith f x $ funOfListsElements f xs

funOfSomeElement :: (a -> a -> a) -> [[a]] -> Int -> a
funOfSomeElement f (x:[]) index = x !! index                             
funOfSomeElement f (x:xs) index = f (x !! index) $ funOfSomeElement f xs index

separateByDate :: Eq a => [(t, a, t1)] -> [[(t, t1)]]
separateByDate xs =
    Data.List.map (\t -> Data.List.map (\(a,b,c)->(a,c)) $
                         Data.List.filter (\(_,b,_)-> (b == t)) xs) timeList

    where timeList = getDateTimeList xs

getDateTimeList :: Eq a => [(t, a, t1)] -> [a]
getDateTimeList xs = Data.List.nub $ Data.List.map (\(a,b,c) -> b) xs

calculateGoodness :: [(String, [Double])] -> Double
calculateGoodness eAscList = averageSquare / n / buyPrice / buyPrice
    where
      averageSquare = Data.List.sum $ Data.List.map (\ x -> (x - buyPrice)^2) sumList
      n = fromIntegral $ length $ e !! 0
      buyPrice = funOfSomeElement (+) e 0
      sumList = funOfListsElements (+) e
      e = Data.List.map snd eAscList