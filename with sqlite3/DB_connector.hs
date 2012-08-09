module DB_connector( connectorAll
                   , connectorCreater
                   , connectorOnce
                   , makeTableHead
                   , separateStringFirst
                   , separateStringMulti
                   , makeContent
                   , turpleToSql
                   , stringsToTurple
                   , stringsToSql
) where

import Database.HDBC

        
connectorAll conn table_name table_head xs = do
  Database.HDBC.run conn ("CREATE TABLE " ++ table_name ++  " (" ++ table_head ++ ")") []
  stmt <- Database.HDBC.prepare conn  ("INSERT INTO " ++ table_name ++ " VALUES (?, ?, ?, ?, ?, ?, ?)")
  Database.HDBC.executeMany stmt xs
  Database.HDBC.commit conn

              
makeTableHead [a0,a1,a2,a3,a4,a5,a6] = a0 ++ " TEXT, " ++ a1 ++ " TEXT, "
                                       ++ a2 ++ " REAL, " ++ a3 ++ " REAL, "
                                       ++ a4 ++ " REAL, " ++ a5 ++ " REAL, " ++ a6 ++ " INT"

makeCurrentHeader = makeTableHead ["symbol","date","open","high","low","close","volume" ]
                                               
