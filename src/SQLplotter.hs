{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
module SQLplotter where

import           Data.String            (fromString)
import           Database.SQLite.Simple
import           System.Directory       (doesFileExist, removeFile)

-- Функция для добавления таблицы сессий
addSessionTable :: IO ()
addSessionTable = do
  dbExists <- doesFileExist "session.db"
  if dbExists
    then removeFile "session.db"
    else return ()

  sqlConnection <- open "session.db"
  execute_
    sqlConnection (fromString $
      "CREATE TABLE IF NOT EXISTS session ("
        ++ "id INTEGER PRIMARY KEY"
        ++ ");"
    )
  close sqlConnection

-- Функция для добавления пользовательской сессии
addUserSession :: Integer -> IO ()
addUserSession userId = do
  sqlConnection <- open "session.db"
  execute sqlConnection "INSERT INTO session (id) VALUES (?)" (Only userId)
  close sqlConnection

-- Функция для получения пользовательской сессии
getUserSession :: IO Integer
getUserSession = do
  sqlConnection <- open "session.db"
  result <- query_ sqlConnection "SELECT id FROM session LIMIT 1;" :: IO [Only Integer]
  close sqlConnection
  case result of
    [Only userId] -> return userId
    _             -> return (-1)

-- Функция инициализации базы данных
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

  -- Создание таблицы addresses
  execute_ sqlConnection (fromString $
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
      ++ ");")

  -- Создание таблицы wallets
  execute_ sqlConnection (fromString $
    "CREATE TABLE IF NOT EXISTS wallets ("
      ++ "id INT PRIMARY KEY,"
      ++ "balance FLOAT"
      ++ ");")

  -- Создание таблицы users
  execute_ sqlConnection (fromString $
    "CREATE TABLE IF NOT EXISTS users ("
      ++ "id INT PRIMARY KEY,"
      ++ "name VARCHAR(255),"
      ++ "surname VARCHAR(255),"
      ++ "email VARCHAR(255),"
      ++ "password VARCHAR(255),"
      ++ "wallet INT,"
      ++ "FOREIGN KEY (wallet) REFERENCES wallets(id)"
      ++ ");")

  -- Создание таблицы ads
  execute_ sqlConnection (fromString $
    "CREATE TABLE IF NOT EXISTS ads ("
      ++ "id INT PRIMARY KEY,"
      ++ "seller INT,"
      ++ "\"objectId\" INT,"
      ++ "\"objectType\" INT,"
      ++ "cost FLOAT,"
      ++ "description TEXT,"
      ++ "FOREIGN KEY (seller) REFERENCES users(id)"
      ++ ");")

  -- Создание таблицы meetings
  execute_ sqlConnection (fromString $
    "CREATE TABLE IF NOT EXISTS meetings ("
      ++ "id INT PRIMARY KEY,"
      ++ "buyer INT,"
      ++ "\"adId\" INT,"
      ++ "date DATE,"
      ++ "FOREIGN KEY (buyer) REFERENCES users(id),"
      ++ "FOREIGN KEY (\"adId\") REFERENCES ads(id)"
      ++ ");")

  -- Создание таблицы deals
  execute_ sqlConnection (fromString $
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
      ++ ");")

  -- Создание таблицы flats
  execute_ sqlConnection (fromString $
    "CREATE TABLE IF NOT EXISTS flats ("
      ++ "id INT PRIMARY KEY,"
      ++ "area FLOAT,"
      ++ "\"roomCount\" INT,"
      ++ "\"addressId\" INT,"
      ++ "floor INT,"
      ++ "\"floorsCount\" INT,"
      ++ "\"balconyArea\" INT,"
      ++ "FOREIGN KEY (\"addressId\") REFERENCES addresses(id)"
      ++ ");")

  -- Создание таблицы houses
  execute_ sqlConnection (fromString $
    "CREATE TABLE IF NOT EXISTS houses ("
      ++ "id INT PRIMARY KEY,"
      ++ "area FLOAT,"
      ++ "\"areType\" INT,"
      ++ "\"addressId\" INT,"
      ++ "\"roomCount\" INT,"
      ++ "\"floorsCount\" INT,"
      ++ "\"basementArea\" INT,"
      ++ "FOREIGN KEY (\"addressId\") REFERENCES addresses(id)"
      ++ ");")

  -- Создание таблицы landPlot
  execute_ sqlConnection (fromString $
    "CREATE TABLE IF NOT EXISTS \"landPlot\" ("
      ++ "id INT PRIMARY KEY,"
      ++ "area FLOAT,"
      ++ "\"landCategory\" INT,"
      ++ "\"addressId\" INT,"
      ++ "FOREIGN KEY (\"addressId\") REFERENCES addresses(id)"
      ++ ");")

  -- Создание таблицы garages
  execute_ sqlConnection (fromString $
    "CREATE TABLE IF NOT EXISTS garages ("
      ++ "id INT PRIMARY KEY,"
      ++ "area INT,"
      ++ "security BOOLEAN,"
      ++ "\"addressId\" INT,"
      ++ "FOREIGN KEY (\"addressId\") REFERENCES addresses(id)"
      ++ ");")

  -- Создание таблицы commercialRealEstates
  execute_ sqlConnection (fromString $
    "CREATE TABLE IF NOT EXISTS \"commercialRealEstates\" ("
      ++ "id INT PRIMARY KEY,"
      ++ "area INT,"
      ++ "\"objectType\" INT,"
      ++ "\"buildingType\" INT,"
      ++ "\"addressId\" INT,"
      ++ "FOREIGN KEY (\"addressId\") REFERENCES addresses(id)"
      ++ ");")

  close sqlConnection

-- Функция для заполнения базы данных начальными данными
seedDB :: IO ()
seedDB = do
  sqlConnection <- open "local.db"

  putStrLn "------------------------------------"
  putStrLn "------------------------------------"
  putStrLn "------ Заполнение базы данных ------"
  putStrLn "------------------------------------"
  putStrLn "------------------------------------"

  -- Вставка данных в таблицу addresses
  execute_ sqlConnection (fromString $
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
      ++ "('Томская область', 'Томск', 'Октябрьский', '100015', 'Улица15', '26', 8, 115);")

  -- Вставка данных в таблицу commercialRealEstates
  execute_ sqlConnection (fromString $
    "INSERT INTO \"commercialRealEstates\" (id, area, \"objectType\", \"buildingType\", \"addressId\") VALUES"
      ++ "(1, 1200, 1, 2, 1),"
      ++ "(2, 2500, 2, 3, 2);")

  -- Вставка данных в таблицу garages
  execute_ sqlConnection (fromString $
    "INSERT INTO garages (id, area, security, \"addressId\") VALUES "
      ++ "(1, 1100, TRUE, 3), "
      ++ "(2, 900, FALSE, 4);")

  -- Вставка данных в таблицу landPlot
  execute_ sqlConnection (fromString $
    "INSERT INTO \"landPlot\" (id, area, \"landCategory\", \"addressId\") VALUES "
      ++ "(1, 3500, 1, 5), "
      ++ "(2, 1900, 3, 6);")

  -- Вставка данных в таблицу houses
  execute_ sqlConnection (fromString $
    "INSERT INTO houses (id, area, \"areType\", \"addressId\", \"roomCount\", \"floorsCount\", \"basementArea\") VALUES "
      ++ "(1, 1320, 2, 7, 6, 2, 0), "
      ++ "(2, 1520, 1, 8, 2, 1, 0);")

  -- Вставка данных в таблицу flats
  execute_ sqlConnection (fromString $
    "INSERT INTO flats (id, area, \"roomCount\", \"addressId\", floor, \"floorsCount\", \"balconyArea\") VALUES "
      ++ "(1, 45, 2, 9, 6, 2, 10), "
      ++ "(2, 34, 1, 10, 2, 1, 0);")

  -- Вставка данных в таблицу wallets
  execute_ sqlConnection (fromString $
    "INSERT INTO wallets (id, balance) VALUES "
      ++ "(1, 100100), "
      ++ "(2, 400000), "
      ++ "(3, 400200), "
      ++ "(4, 100200), "
      ++ "(5, 100000), "
      ++ "(6, 100000);")

  -- Вставка данных в таблицу users
  execute_ sqlConnection (fromString $
    "INSERT INTO users (id, name, surname, email, password, wallet) VALUES "
      ++ "(1, 'Egor', 'Zhvakin', 'egor.zhvakin@mail.ru', 'pass', 1), "
      ++ "(2, 'Stanislav', 'Hohlov', 'ssstttaaasssiiikk@mail.ru', 'pass', 2), "
      ++ "(3, 'Bogdan', 'Eremin', 'bodya43@mail.ru', 'pass', 3), "
      ++ "(4, 'Miron', 'Neborski', 'miroooosha@mail.ru', 'pass', 4), "
      ++ "(5, 'Vladislav', 'Verkholansev', 'verxolancev@mail.ru', 'pass', 5), "
      ++ "(6, 'Daniil', 'Ivanichev', 'yaneidd@mail.ru', 'pass', 6);")

  -- Вставка данных в таблицу ads с описаниями
  execute_ sqlConnection (fromString $
    "INSERT INTO ads (id, seller, \"objectId\", \"objectType\", cost, description) VALUES "
      ++ "(1, 1, 1, 1, 10000, 'Продается уютная квартира в центре города.'), "
      ++ "(2, 2, 2, 1, 15000, 'Большая квартира с двумя балконами.'), "
      ++ "(3, 3, 3, 2, 20000, 'Современный дом с садом и гаражом.'), "
      ++ "(4, 4, 4, 3, 25000, 'Земельный участок под строительство дома.'), "
      ++ "(5, 1, 5, 4, 30000, 'Гараж с автоматической дверью.'), "
      ++ "(6, 2, 6, 1, 12000, 'Квартира рядом с парком.'), "
      ++ "(7, 3, 7, 2, 18000, 'Дом с бассейном и сауной.'), "
      ++ "(8, 4, 8, 3, 22000, 'Земельный участок в экологически чистом районе.');")

  -- Вставка данных в таблицу deals
  execute_ sqlConnection (fromString $
    "INSERT INTO deals (id, buyer, \"adId\", \"dealType\", status, date, \"finalCost\") VALUES "
      ++ "(1, 2, 5, 1, 'new', '2024-08-16T09:00:00.000Z', 10000), "
      ++ "(2, 1, 6, 1, 'close', '2024-08-16T09:00:00.000Z', 20000), "
      ++ "(3, 4, 7, 1, 'reject', '2024-08-16T09:00:00.000Z', 100000000), "
      ++ "(4, 1, 8, 1, 'new', '2024-08-16T09:00:00.000Z', 17500000), "
      ++ "(5, 1, 1, 1, 'close', '2024-08-16T09:00:00.000Z', 67000);")

  close sqlConnection
