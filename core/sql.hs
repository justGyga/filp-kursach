module Database where

import Data.String (fromString)
import Data.Text (Text)
import Database.SQLite.Simple

-- Открытие соединения с базой данных SQLite
connectToDB :: IO Connection
connectToDB = open "app.db"

-- Создание таблицы manufacturers
createManufacturersTable :: Connection -> IO ()
createManufacturersTable conn = do
  execute_ conn (fromString "CREATE TABLE IF NOT EXISTS manufacturers (id INTEGER PRIMARY KEY, name TEXT);")

-- Вставка данных в таблицу manufacturers
insertManufacturer :: Connection -> Text -> IO ()
insertManufacturer conn name = do
  execute conn (fromString "INSERT INTO manufacturers (name) VALUES (?)") (Only name)