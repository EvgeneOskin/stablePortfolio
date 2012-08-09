module SRC.App.PoolGenerator ( dirSearchForCsvFiles
                             , mainGetLoop
                             , makeTotalPool
) where

import SRC.App.UsefullFunc
import System.IO
import System.FilePath ((</>))
import System.Directory
import Data.List
import Control.Monad (forM)
import Data.Map (toAscList)

makeTotalPool :: String -> String -> [String] -> IO [(String, [Double])]
makeTotalPool db_driver dbFile timePeriod = do
  allCsvFiles <- dirSearchForCsvFiles inDN market
  let csvFilesInNeededTime = filter (\x->pickDates x market timePeriod) allCsvFiles
  forM sqlFilesInNededTime (\a -> do
                              system $ db_driver ++ " " ++ dbFile ++ " < \"" a ++ "\"")
  conn <- Database.HDBC.Sqlite3.connectSqlite3 dbFile
  fileData <- getDataFromSqlDB conn timePeriod
  let totalData = toAscList $ concatCsvData $ getDateList filesData
  return totalData
                                   
dirSearchForCsvFiles :: String -> String -> String -> IO [String]
dirSearchForCsvFiles topDirI market suffixFileName = do
  --suffixFileName = ".sql"
  topDirContents <- System.Directory.getDirectoryContents topDirI
  let properNames = filter (`notElem` [".",".."]) topDirContents
      fileNames = filter (market `isPrefixOf`) $ filter (suffixFileName `isSuffixOf`) properNames
      notNeedfileNames = filter (`notElem` fileNames) properNames
  xs <- forM notNeedileNames $ \name -> do
                                let pathI = topDirI </> name
                                isDir <- System.Directory.doesDirectoryExist pathI
                                if isDir
                                  then do
                                    deepFiles <- dirSearchForCsvFiles pathI market
                                    return $ unlines deepFiles
                                  else  return ""
  let inDirFileNames = filter (`notElem` [""]) xs
      totalFilesList = (map (topDirI </>) fileNames) ++ (concat $ map lines inDirFileNames)
  return totalFilesList
         
getDataFromSqlDB :: FilePath -> IO [(String, Double)]
getDataFromSqlDB conn [low, hi]= do
  let quereStr = "SELECT ticker, date, closePrice FROM prices WHERE (date < "
                 ++ hiStr ++") AND (date > " ++ lowStr ++ ")"
      hiStr = changeDateFormat hi "%Y%m%d" "%Y-%m-%d %H:%M:%S"
      lowStr = changeDateFormat low "%Y%m%d" "%Y-%m-%d %H:%M:%S"
  allSqlData <- quickQuery' conn quereStr --contain all symbols... need to get symbol and [all prices on time period....]
  let allDataList = map (\a,b,c -> (fromSql a, fromSql b, fromSql c)) allSqlData