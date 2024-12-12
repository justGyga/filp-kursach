{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
module SQLplotter where

import Data.String (fromString)
import Database.SQLite.Simple
import System.Directory (doesFileExist, removeFile)

addSessionTable :: IO ()
addSessionTable = do
  dbExists <- doesFileExist "session.db"
  if dbExists
    then do
      removeFile "session.db"
    else return ()

  sqlConnection <- open "session.db"
  execute_
    sqlConnection (fromString $
      "CREATE TABLE IF NOT EXISTS session ("
        ++"id INTEGER PRIMARY KEY"
        ++");"
    )
  close sqlConnection

addUserSession :: Integer -> IO ()
addUserSession id = do
  sqlConnection <- open "session.db"
  execute sqlConnection "INSERT INTO session (id) VALUES (?)" (Only id)
  close sqlConnection

getUserSession :: IO Integer
getUserSession = do
  sqlConnection <- open "session.db"
  result <- query_ sqlConnection "SELECT id FROM session LIMIT 1;"
  close sqlConnection
  case result of
    [Only id] -> return id
    _         -> return (-1)

initializeDB :: IO ()
initializeDB = do
  dbExists <- doesFileExist "local.db"
  if dbExists
    then do
      putStrLn "------------------------------------"
      putStrLn "------------------------------------"
      putStrLn "---------- Очищение таблиц ---------"
      putStrLn "------------------------------------"
      putStrLn "------------------------------------"
      removeFile "local.db"
    else return ()

  sqlConnection <- open "local.db"
  putStrLn "------------------------------------"
  putStrLn "------------------------------------"
  putStrLn "---------- Создание таблиц ---------"
  putStrLn "------------------------------------"
  putStrLn "------------------------------------"

  execute_ -- made
    sqlConnection
    ( fromString $
        "CREATE TABLE IF NOT EXISTS addresses ("
          ++ "id INTEGER PRIMARY KEY,"
          ++ "state VARCHAR(100) NOT NULL,"
          ++ "city VARCHAR(100) NOT NULL,"
          ++ "district VARCHAR(100) NOT NULL,"
          ++ "\"postalCode\" VARCHAR(20),"
          ++ "\"streetName\" VARCHAR(255) NOT NULL,"
          ++ "\"houseNumber\" VARCHAR(50),"
          ++ "entrance INTEGER,"
          ++ "\"doorNumber\" INTEGER"
          ++ ");"
    )
  execute_ -- made
    sqlConnection
    (fromString "CREATE TABLE IF NOT EXISTS wallets ( id INT PRIMARY KEY, balance FLOAT);")

  execute_ -- made
    sqlConnection
    ( fromString $
        "CREATE TABLE IF NOT EXISTS users ("
          ++ "id INT PRIMARY KEY,"
          ++ "name VARCHAR(255),"
          ++ "surname VARCHAR(255),"
          ++ "email VARCHAR(255),"
          ++ "password VARCHAR(255),"
          ++ "wallet INT,"
          ++ "FOREIGN KEY (wallet) REFERENCES wallets(id)"
          ++ ");"
    )

  execute_ -- made
    sqlConnection
    ( fromString $
        "CREATE TABLE IF NOT EXISTS ads ("
          ++ "id INT PRIMARY KEY,"
          ++ "seller INT,"
          ++ "\"objectId\" INT,"
          ++ "\"objectType\" INT,"
          ++ "cost FLOAT,"
          ++ "description TEXT,"
          ++ "FOREIGN KEY (seller) REFERENCES users(id)"
          ++ ");"
    )

  execute_
    sqlConnection
    ( fromString $
        "CREATE TABLE IF NOT EXISTS meetings ("
          ++ "id INT PRIMARY KEY,"
          ++ "buyer INT,"
          ++ "\"adId\" INT,"
          ++ "date DATE,"
          ++ "FOREIGN KEY (buyer) REFERENCES users(id),"
          ++ "FOREIGN KEY (\"adId\") REFERENCES ads(id)"
          ++ ");"
    )

  execute_
    sqlConnection
    ( fromString $
        "CREATE TABLE IF NOT EXISTS deals ("
          ++ "id INT PRIMARY KEY,"
          ++ "buyer INT,"
          ++ "\"adId\" INT,"
          ++ "\"dealType\" VARCHAR(255),"
          ++ "status VARCHAR(255),"
          ++ "date DATE,"
          ++ "\"finalCost\" FLOAT,"
          ++ "FOREIGN KEY (buyer) REFERENCES users(id),"
          ++ "FOREIGN KEY (\"adId\") REFERENCES ads(id)"
          ++ ");"
    )

  execute_ -- made
    sqlConnection
    ( fromString $
        "CREATE TABLE IF NOT EXISTS flats ("
          ++ "id INT PRIMARY KEY,"
          ++ "area FLOAT,"
          ++ "\"roomCount\" INT,"
          ++ "\"addressId\" INT,"
          ++ "floor INT,"
          ++ "\"floorsCount\" INT,"
          ++ "\"balconyArea\" INT,"
          ++ "FOREIGN KEY (\"addressId\") REFERENCES addresses(id)"
          ++ ");"
    )

  execute_ -- made
    sqlConnection
    ( fromString $
        "CREATE TABLE IF NOT EXISTS houses ("
          ++ "id INT PRIMARY KEY,"
          ++ "area FLOAT,"
          ++ "\"areType\" INT,"
          ++ "\"addressId\" INT,"
          ++ "\"roomCount\" INT,"
          ++ "\"floorsCount\" INT,"
          ++ "\"basementArea\" INT,"
          ++ "FOREIGN KEY (\"addressId\") REFERENCES addresses(id)"
          ++ ");"
    )

  execute_ -- made
    sqlConnection
    ( fromString $
        "CREATE TABLE IF NOT EXISTS \"landPlot\" ("
          ++ "id INT PRIMARY KEY,"
          ++ "area FLOAT,"
          ++ "\"landCategory\" INT,"
          ++ "\"addressId\" INT,"
          ++ "FOREIGN KEY (\"addressId\") REFERENCES addresses(id)"
          ++ ");"
    )

  execute_ -- made
    sqlConnection
    ( fromString $
        "CREATE TABLE IF NOT EXISTS garages ("
          ++ "id INT PRIMARY KEY,"
          ++ "area INT,"
          ++ "security BOOLEAN,"
          ++ "\"addressId\" INT,"
          ++ "FOREIGN KEY (\"addressId\") REFERENCES addresses(id)"
          ++ ");"
    )

  execute_ -- made
    sqlConnection
    ( fromString $
        "CREATE TABLE IF NOT EXISTS \"commercialRealEstates\" ("
          ++ "id INT PRIMARY KEY,"
          ++ "area INT,"
          ++ "\"objectType\" INT,"
          ++ "\"buildingType\" INT,"
          ++ "\"addressId\" INT,"
          ++ "FOREIGN KEY (\"addressId\") REFERENCES addresses(id)"
          ++ ");"
    )
  
  close sqlConnection

seedDB :: IO ()
seedDB = do
  sqlConnection <- open "local.db"

  putStrLn "------------------------------------"
  putStrLn "------------------------------------"
  putStrLn "------ Заполнение базы данных ------"
  putStrLn "------------------------------------"
  putStrLn "------------------------------------"

  execute_
    sqlConnection
    ( fromString $
        "INSERT INTO addresses (state, city, district, \"postalCode\", \"streetName\", \"houseNumber\", entrance, \"doorNumber\") VALUES "
          ++ "('Томская область', 'Томск', 'Кировский', '100001', 'Улица1', '12', 1, 101),"
          ++ "('Томская область', 'Томск', 'Кировский', '100002', 'Улица2', '13', 1, 102),"
          ++ "('Томская область', 'Томск', 'Ленинский', '100003', 'Улица3', '14', 2, 103),"
          ++ "('Томская область', 'Томск', 'Ленинский', '100004', 'Улица4', '15', 2, 104),"
          ++ "('Томская область', 'Томск', 'Ленинский', '100005', 'Улица5', '16', 3, 105),"
          ++ "('Томская область', 'Томск', 'Ленинский', '100019', 'Улица19', '30', 10, 119),"
          ++ "('Томская область', 'Томск', 'Ленинский', '100018', 'Улица18', '29', 9, 118),"
          ++ "('Томская область', 'Томск', 'Ленинский', '100020', 'Улица20', '31', 10, 120),"
          ++ "('Томская область', 'Томск', 'Советский', '100010', 'Улица10', '21', 5, 110),"
          ++ "('Томская область', 'Томск', 'Советский', '100011', 'Улица11', '22', 6, 111),"
          ++ "('Томская область', 'Томск', 'Советский', '100012', 'Улица12', '23', 6, 112),"
          ++ "('Томская область', 'Томск', 'Советский', '100016', 'Улица16', '27', 8, 116),"
          ++ "('Томская область', 'Томск', 'Советский', '100017', 'Улица17', '28', 9, 117),"
          ++ "('Томская область', 'Томск', 'Октябрьский', '100006', 'Улица6', '17', 3, 106),"
          ++ "('Томская область', 'Томск', 'Октябрьский', '100007', 'Улица7', '18', 4, 107),"
          ++ "('Томская область', 'Томск', 'Октябрьский', '100008', 'Улица8', '19', 4, 108),"
          ++ "('Томская область', 'Томск', 'Октябрьский', '100009', 'Улица9', '20', 5, 109),"
          ++ "('Томская область', 'Томск', 'Октябрьский', '100013', 'Улица13', '24', 7, 113),"
          ++ "('Томская область', 'Томск', 'Октябрьский', '100014', 'Улица14', '25', 7, 114),"
          ++ "('Томская область', 'Томск', 'Октябрьский', '100015', 'Улица15', '26', 8, 115);"
    )

  execute_
    sqlConnection
    ( fromString $
        "INSERT INTO \"commercialRealEstates\" (id, area, \"objectType\", \"buildingType\", \"addressId\") VALUES"
          ++ "(1, 1200, 1, 2, 1),"
          ++ "(2, 2500, 2, 3, 2);"
    )

  execute_
    sqlConnection
    (fromString "INSERT INTO garages (id, area, security, \"addressId\") VALUES (1, 1100, TRUE, 3), (2, 900, FALSE, 4);")

  execute_
    sqlConnection
    (fromString "INSERT INTO \"landPlot\" (id, area, \"landCategory\", \"addressId\") VALUES (1, 3500, 1, 5), (2, 1900, 3, 6);")

  execute_
    sqlConnection
    ( fromString $
        "INSERT INTO houses (id, area, \"areType\", \"addressId\", \"roomCount\", \"floorsCount\",\"basementArea\") VALUES"
          ++ "(1, 1320, 2, 7, 6, 2, 0),"
          ++ "(2, 1520, 1, 8, 2, 1, 0);"
    )

  execute_
    sqlConnection
    ( fromString $
        "INSERT INTO flats (id, area, \"roomCount\", \"addressId\", floor, \"floorsCount\",\"balconyArea\") VALUES"
          ++ "(1, 45, 2, 9, 6, 2, 10),"
          ++ "(2, 34, 1, 10, 2, 1, 0);"
    )

  execute_
    sqlConnection
    ( fromString $
        "INSERT INTO wallets (id, balance) VALUES"
          ++ "(1, 100100),"
          ++ "(2, 400000),"
          ++ "(3, 400200),"
          ++ "(4, 100200),"
          ++ "(5, 100000),"
          ++ "(6, 100000);"
    )

  execute_
    sqlConnection
    ( fromString $
        "INSERT INTO users (id, name, surname, email, password, wallet) VALUES"
          ++ "(1, 'Egor', 'Zhvakin', 'egor.zhvakin@mail.ru', 'pass', 1),"
          ++ "(2, 'Stanislav', 'Hohlov', 'ssstttaaasssiiikk@mail.ru', 'pass', 2),"
          ++ "(3, 'Bogdan', 'Eremin', 'bodya43@mail.ru', 'pass', 3),"
          ++ "(4, 'Miron', 'Neborski', 'miroooosha@mail.ru', 'pass', 4),"
          ++ "(5, 'Vladislav', 'Verkholansev', 'verxolancev@mail.ru', 'pass', 5),"
          ++ "(6, 'Daniil', 'Ivanichev', 'yaneidd@mail.ru', 'pass', 6);"
    )

  execute_
    sqlConnection
    ( fromString $
        "INSERT INTO ads (id, seller, \"objectId\", \"objectType\", cost) VALUES"
          ++ "( 1, 1, 1, 1, 10000),"
          ++ "( 2, 2, 2, 1, 10000),"
          ++ "( 3, 3, 1, 2, 10000),"
          ++ "( 4, 4, 2, 2, 10000),"
          ++ "( 5, 1, 1, 3, 10000),"
          ++ "( 6, 2, 2, 3, 10000),"
          ++ "( 7, 3, 1, 4, 10000),"
          ++ "( 8, 4, 2, 4, 10000);"
    )

  execute_
    sqlConnection
    ( fromString $
        "INSERT INTO meetings (id, buyer, \"adId\", date) VALUES"
          ++ "( 1, 2, 1, '2024-08-16T09:00:00.000Z'),"
          ++ "( 2, 3, 1, '2024-09-15T10:00:00.000Z'),"
          ++ "( 3, 4, 1, '2024-10-09T16:00:00.000Z'),"
          ++ "( 4, 1, 2, '2024-11-02T15:00:00.000Z'),"
          ++ "( 5, 2, 3, '2024-11-10T15:00:00.000Z'),"
          ++ "( 6, 4, 4, '2024-12-15T23:00:00.000Z');"
    )

  execute_
    sqlConnection
    ( fromString $
        "INSERT INTO deals (id, buyer, \"adId\", \"dealType\", status, date, \"finalCost\") VALUES"
          ++ "( 1, 2, 5, 1, 'new', '2024-08-16T09:00:00.000Z', 10000),"
          ++ "( 2, 1, 6, 1, 'close', '2024-08-16T09:00:00.000Z', 20000),"
          ++ "( 3, 4, 7, 1, 'reject', '2024-08-16T09:00:00.000Z', 100000000),"
          ++ "( 4, 1, 8, 1, 'new', '2024-08-16T09:00:00.000Z', 17500000),"
          ++ "( 5, 1, 1, 1, 'close', '2024-08-16T09:00:00.000Z', 67000);"
    )

  close sqlConnection