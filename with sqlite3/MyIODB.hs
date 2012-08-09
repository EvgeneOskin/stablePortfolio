module MyIO
    (
      input_greeting,
      ioData,
      convertSingle,
      mainloop,
      dirSearch,
      isCSVFile
    ) where

    import Database.HDBC
    import Database.HDBC.Sqlite3
    import System.Environment (getArgs)
    import DB_connector
    import System.IO
    import Text.Regex.Posix
    import Control.Monad (forM)
    import System.Directory
    import System.FilePath ((</>))
        
    input_greeting = do
      args <- getArgs
      case args of
        [input,output] -> putStrLn "all good"
        _ -> putStrLn "error: plese  type in cmd line 2 file name"

    convertSingle input output = do
      inh <- openFile input ReadMode
      conn <- Database.HDBC.Sqlite3.connectSqlite3 output
      headString <- hGetLine inh
      let
          head = DB_connector.makeTableHead (DB_connector.separateStringMulti (==',') headString)
      DB_connector.connectorCreater conn "t1" head
      mainloop inh conn "t1"
      hClose inh
      Database.HDBC.disconnect conn
      return True
                      
    mainloop inh conn table_name = do 
      ineof <- hIsEOF inh
      if ineof
      then return ()
      else do
        inpStr <- hGetLine inh
        let
            sqlrow = DB_connector.stringsToSql (DB_connector.separateStringMulti (==',') inpStr)
        connectorOnce conn table_name  sqlrow
        mainloop inh conn table_name
        
    ioData name_db headString contentString = do
      let 
          sqlContentMaker = map (DB_connector.stringsToSql)
          sqlContent = sqlContentMaker (DB_connector.makeContent contentString)
          head = DB_connector.makeTableHead (DB_connector.separateStringMulti (==',') headString)
      DB_connector.connectorAll name_db "table_1" head sqlContent

    dirSearch topDirI topDirO = do
      topDirContents <- System.Directory.getDirectoryContents topDirI
      let properNames = filter (`notElem` [".",".."]) topDirContents
      forM properNames $ \name ->
          do
            let pathI = topDirI </> name
                pathO = topDirO </> name
            isDir <- System.Directory.doesDirectoryExist pathI
            if isDir
            then
                do
                  System.Directory.createDirectoryIfMissing True pathO
                  dirSearch pathI pathO
                  return True
            else
                do
                  isCSV <- isCSVFile pathI
                  if isCSV
                  then convertSingle pathI (nameCSVtoSQL pathO)
                  else return False
                  
    isCSVFile x = return (xStr =~ "(.)+[.]csv" :: Bool)
        where xStr = x :: String

    nameCSVtoSQL x =  take (length x - 3) x  ++ "sql"
