module MyDB (Column(..), ColumnType(..), Cell(..), Record, Table(..), QueryResult,
 createTable, getTables) where

import System.Directory

type ColumnName = String
data ColumnType = DBString | DBNumber deriving (Enum, Show, Read)
data Column = Column {
  cname  :: ColumnName,
  ctype :: ColumnType
} deriving (Show, Read)


data Cell a = Cell (Column, a) deriving (Show, Read)
type Record a = [Cell a]
data Table = Table {
  tname :: String,
  tstructure :: [Column]
} deriving (Show, Read)

type QueryResult = Either


createTable :: String -> [Column] -> IO()
createTable name columns = do
  let table = Table { tname = name, tstructure = columns}
  tables <- getTables
  writeFile "./storage" $ show $ table : tables

getTables :: IO [Table]
getTables = do
  let filename = "./storage"
  fileExist <- doesFileExist fileName
  if not fileExist
  then return []
  else do
    str <- readFile filename
    return (read str :: [Table])






