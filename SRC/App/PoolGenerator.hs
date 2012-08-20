module SRC.App.PoolGenerator (makeTotalPool
) where

import SRC.App.UsefullFunc
import Data.List
import Data.Map (toAscList)
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad (forM)
    
makeTotalPool :: String -> String -> [String] -> IO [(String, [Double])]
makeTotalPool _ dbFile timePeriod = do
  conn <- Database.HDBC.Sqlite3.connectSqlite3 dbFile
  fileData <- getDataFromSqlDB conn timePeriod
  let totalData = (toAscList . (concatData . separateByDate)) fileData
  Database.HDBC.disconnect conn
  return totalData
              
getDataFromSqlDB :: Connection -> [String]-> IO [(String, String, Double)]
getDataFromSqlDB conn [low, hi]= do
  let quereStr = "SELECT ticker, date, closePrice FROM prices WHERE (date < "
                 ++ hiStr ++") AND (date > " ++ lowStr ++ ")"
      hiStr  = changeDateFormat hi  "%Y%m%d" "\"%Y-%m-%d %H:%M:%S\""
      lowStr = changeDateFormat low "%Y%m%d" "\"%Y-%m-%d %H:%M:%S\""
  allSqlData <- Database.HDBC.quickQuery' conn quereStr []
  let allDataList = map (\[a, b, c] -> (fromSql a, fromSql b, fromSql c)) allSqlData :: [(String, String, Double)]
  return allDataList