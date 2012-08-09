module SRC.App.UsefullFunc ( changeDateFormat
                           , getDataFromStr
                           , pickDates
                           , concatCsvData
                           , funOfListsElements
                           , funOfSomeElement
                           , separateByDate
) where

  
import Data.Time
import Data.Time.Format
import System.Locale
import Data.List.Split
import Data.Map
import Data.List

changeDateFormat :: String -> String -> String -> String
changeDateFormat x haveFx needFx = Data.Time.Format.formatTime System.Locale.defaultTimeLocale needFx t
    where t = Data.Time.Format.readTime System.Locale.defaultTimeLocale haveFx x :: Data.Time.UTCTime

getDataFromStr :: String -> (String, Double)
getDataFromStr str = (symbol, price)
    where splitedStr = endBy "," str
          symbol = head splitedStr
          prices = Prelude.map read (init (drop 2 splitedStr)) :: [Double]
          price = last prices
                   
pickDates :: String -> String -> [String] -> Bool
pickDates x market [low, hi] = (strForCompare >= low) && (strForCompare <= hi)
    where strForCompare = changeDateFormat (last $ endBy "/" x) (market ++ "_%Y%m%d.csv") "%Y%m%d"

concatData :: Ord k => [[(k, a)]] -> Data.Map.Map k [a]
concatData (x:[]) = Data.Map.map (\a -> [a]) $ fromList x
concatData (x:xs) = intersectionWith (\a b -> [a] ++ b) (fromList x) $ concatCsvData xs

funOfListsElements :: (b -> b -> b) -> [[b]] -> [b] 
funOfListsElements f (x:[]) = x
funOfListsElements f (x:xs) = zipWith f x $ funOfListsElements f xs

funOfSomeElement :: (a -> a -> a) -> [[a]] -> Int -> a
funOfSomeElement f (x:[]) index = x !! index                             
funOfSomeElement f (x:xs) index = f (x !! index) $ funOfSomeElement f xs index

separateByDate xs =
    Data.List.map (\t -> Data.List.map (\(a,b,c)->(a,c)) $
                         Data.List.filter (\(_,b,_)-> (b == t)) xs) timeList

    where timeList = getDateTimeList xs

getDateTimeList xs = Data.List.nub $ Data.List.map (\(a,b,c) -> b) xs