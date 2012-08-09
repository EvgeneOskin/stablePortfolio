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
makeTotalPool market inDN timePeriod = do
  allCsvFiles <- dirSearchForCsvFiles inDN market
  let csvFilesInNeededTime = filter (\x->pickDates x market timePeriod) allCsvFiles
  csvFilesData <- forM csvFilesInNeededTime getDataFromCsv
  let totalData = toAscList $ concatCsvData csvFilesData
  return totalData
                                   
dirSearchForCsvFiles :: String -> String -> IO [String]
dirSearchForCsvFiles topDirI market = do
  topDirContents <- System.Directory.getDirectoryContents topDirI
  let properNames = filter (`notElem` [".",".."]) topDirContents
      csvFileNames = filter (market `isPrefixOf`) $ filter (".csv" `isSuffixOf`) properNames
      notCsvFileNames = filter (`notElem` csvFileNames) properNames
  xs <- forM notCsvFileNames $ \name -> do
                                let pathI = topDirI </> name
                                isDir <- System.Directory.doesDirectoryExist pathI
                                if isDir
                                  then do
                                    csvDeepFiles <- dirSearchForCsvFiles pathI market
                                    return $ unlines csvDeepFiles
                                  else  return ""
  let inDirCsvFileNames = filter (`notElem` [""]) xs
      totalCsvFilesList = (map (topDirI </>) csvFileNames) ++ (concat $ map lines inDirCsvFileNames)
  return totalCsvFilesList
         
getDataFromCsv :: FilePath -> IO [(String, Double)]
getDataFromCsv inFN = do
  inh <- openFile inFN ReadMode
  csvData <- mainGetLoop inh
  hClose inh
  return csvData

mainGetLoop :: Handle -> IO [(String, Double)]
mainGetLoop inh = do
  ineof <- hIsEOF inh
  if ineof
    then return []
    else do
      inpStr <- hGetLine inh
      dataRows <- mainGetLoop inh
      let currentDataRows = (getDataFromStr inpStr):dataRows
      return currentDataRows