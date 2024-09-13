{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module PCClient
  ( getConnection,
    createTable,
    insertDummyData,
    fetchUsers,
  )
where

import Database.PostgreSQL.Simple (ConnectInfo, Connection, connect, connectDatabase, connectHost, connectPassword, connectPort, connectUser, defaultConnectInfo, execute_, execute, query_, query)
import Database.PostgreSQL.Simple.FromRow
import Data.ByteString.Char8 qualified as BSC

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


insertDummyData :: Connection -> (BSC.ByteString, Int) -> IO ()
insertDummyData conn (name, age) = do
  let insertQuery = "INSERT INTO public.users (name, age) VALUES (?, ?);"
  -- Inserting dummy data
  _ <- execute conn insertQuery (name, age)
  putStrLn "Dummy data inserted."

fetchUsers :: Connection -> IO [User]
fetchUsers conn = do
  let selectQuery = "SELECT id, name, age FROM public.users;"
  query_ conn selectQuery
