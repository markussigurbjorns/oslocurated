{-# LANGUAGE OverloadedStrings #-}

module PCClient
  ( getConnection,
    createTable,
    insertDummyData,
    fetchUsers,
  )
where

import Database.PostgreSQL.Simple (ConnectInfo, Connection, connect, connectDatabase, connectHost, connectPassword, connectPort, connectUser, defaultConnectInfo, execute_, execute, query_, query)
import Database.PostgreSQL.Simple.FromRow
import Data.ByteString.Char8 (ByteString, pack)

data User = User { userId :: Int, userName :: String, userAge :: Int } deriving (Show)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field


getConnection :: IO Connection
getConnection = connect dbInfo

dbInfo :: ConnectInfo
dbInfo =
  defaultConnectInfo
    { connectHost = "20.82.1.112",
      connectPort = 5433,
      connectUser = "oslocuratedpsql",
      connectPassword = "OsloCurated01",
      connectDatabase = "oslocurateddb"
    }

createTable :: Connection -> IO ()
createTable conn = do
  let createTableQuery =
        "CREATE TABLE IF NOT EXISTS public.users ( \
        \ id SERIAL PRIMARY KEY, \
        \ name VARCHAR(100), \
        \ age INT \
        \ );"
  _ <- execute_ conn createTableQuery
  putStrLn "Table created."


insertDummyData :: Connection -> IO ()
insertDummyData conn = do
  let insertQuery = "INSERT INTO public.users (name, age) VALUES (?, ?);"
  -- Inserting dummy data
  _ <- execute conn insertQuery (pack "Alice", 25 :: Int)
  _ <- execute conn insertQuery (pack "Bob", 30 :: Int)
  _ <- execute conn insertQuery (pack "Charlie", 22 :: Int)
  putStrLn "Dummy data inserted."

fetchUsers :: Connection -> IO [User]
fetchUsers conn = do
  let selectQuery = "SELECT id, name, age FROM public.users;"
  query_ conn selectQuery
