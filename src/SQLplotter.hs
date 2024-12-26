{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use when" #-}
module SQLplotter where

import Data.String (fromString)
import Database.SQLite.Simple
import System.Directory (doesFileExist, removeFile)

-- Функция для добавления таблицы сессий
addSessionTable :: IO ()
addSessionTable = do
  dbExists <- doesFileExist "session.db"
  if dbExists
    then removeFile "session.db"
    else return ()

  sqlConnection <- open "session.db"
  execute_
    sqlConnection
    ( fromString $
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
    _ -> return (-1)

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

  -- Создание таблиц (адреса, wallets, users, ads, meetings, deals, flats, houses, landPlot, garages, commercialRealEstates)
  execute_
    sqlConnection
    ( fromString $
        "CREATE TABLE IF NOT EXISTS addresses ("
          ++ "id INTEGER PRIMARY KEY AUTOINCREMENT,"
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

  execute_
    sqlConnection
    ( fromString $
        "CREATE TABLE IF NOT EXISTS wallets ("
          ++ "id INTEGER PRIMARY KEY AUTOINCREMENT,"
          ++ "balance FLOAT"
          ++ ");"
    )

  execute_
    sqlConnection
    ( fromString $
        "CREATE TABLE IF NOT EXISTS users ("
          ++ "id INTEGER PRIMARY KEY AUTOINCREMENT,"
          ++ "name VARCHAR(255),"
          ++ "surname VARCHAR(255),"
          ++ "email VARCHAR(255),"
          ++ "password VARCHAR(255),"
          ++ "wallet INTEGER,"
          ++ "FOREIGN KEY (wallet) REFERENCES wallets(id)"
          ++ ");"
    )

  execute_
    sqlConnection
    ( fromString $
        "CREATE TABLE IF NOT EXISTS ads ("
          ++ "id INTEGER PRIMARY KEY AUTOINCREMENT,"
          ++ "seller INTEGER,"
          ++ "\"objectId\" INTEGER,"
          ++ "\"objectType\" INTEGER,"
          ++ "cost FLOAT,"
          ++ "description TEXT,"
          ++ "FOREIGN KEY (seller) REFERENCES users(id)"
          ++ ");"
    )

  execute_
    sqlConnection
    ( fromString $
        "CREATE TABLE IF NOT EXISTS meetings ("
          ++ "id INTEGER PRIMARY KEY AUTOINCREMENT,"
          ++ "buyer INTEGER,"
          ++ "\"adId\" INTEGER,"
          ++ "date DATE,"
          ++ "FOREIGN KEY (buyer) REFERENCES users(id),"
          ++ "FOREIGN KEY (\"adId\") REFERENCES ads(id)"
          ++ ");"
    )

  execute_
    sqlConnection
    ( fromString $
        "CREATE TABLE IF NOT EXISTS deals ("
          ++ "id INTEGER PRIMARY KEY AUTOINCREMENT,"
          ++ "buyer INTEGER,"
          ++ "\"adId\" INTEGER,"
          ++ "\"dealType\" VARCHAR(255),"
          ++ "status VARCHAR(255),"
          ++ "date DATE,"
          ++ "\"finalCost\" FLOAT,"
          ++ "FOREIGN KEY (buyer) REFERENCES users(id),"
          ++ "FOREIGN KEY (\"adId\") REFERENCES ads(id)"
          ++ ");"
    )

  execute_
    sqlConnection
    ( fromString $
        "CREATE TABLE IF NOT EXISTS flats ("
          ++ "id INTEGER PRIMARY KEY AUTOINCREMENT,"
          ++ "area INT,"
          ++ "\"roomCount\" INT,"
          ++ "\"addressId\" INTEGER,"
          ++ "floor INT,"
          ++ "\"floorsCount\" INT,"
          ++ "\"balconyArea\" INT,"
          ++ "\"ot\" INTEGER DEFAULT 1, "
          ++ "FOREIGN KEY (\"addressId\") REFERENCES addresses(id)"
          ++ ");"
    )

  execute_
    sqlConnection
    ( fromString $
        "CREATE TABLE IF NOT EXISTS houses ("
          ++ "id INTEGER PRIMARY KEY AUTOINCREMENT,"
          ++ "area INTEGER,"
          ++ "\"areaType\" INTEGER,"
          ++ "\"addressId\" INTEGER,"
          ++ "\"roomCount\" INTEGER,"
          ++ "\"floorsCount\" INTEGER,"
          ++ "\"basementArea\" INTEGER,"
          ++ "\"ot\" INTEGER DEFAULT 2, "
          ++ "FOREIGN KEY (\"addressId\") REFERENCES addresses(id)"
          ++ ");"
    )

  execute_
    sqlConnection
    ( fromString $
        "CREATE TABLE IF NOT EXISTS \"landPlot\" ("
          ++ "id INTEGER PRIMARY KEY AUTOINCREMENT,"
          ++ "area INT,"
          ++ "\"landCategory\" INT,"
          ++ "\"addressId\" INTEGER,"
          ++ "\"ot\" INTEGER DEFAULT 3, "
          ++ "FOREIGN KEY (\"addressId\") REFERENCES addresses(id)"
          ++ ");"
    )

  execute_
    sqlConnection
    ( fromString $
        "CREATE TABLE IF NOT EXISTS garages ("
          ++ "id INTEGER PRIMARY KEY AUTOINCREMENT,"
          ++ "area INT,"
          ++ "security BOOLEAN,"
          ++ "\"addressId\" INTEGER,"
          ++ "\"ot\" INTEGER DEFAULT 4, "
          ++ "FOREIGN KEY (\"addressId\") REFERENCES addresses(id)"
          ++ ");"
    )

  execute_
    sqlConnection
    ( fromString $
        "CREATE TABLE IF NOT EXISTS \"commercialRealEstates\" ("
          ++ "id INTEGER PRIMARY KEY AUTOINCREMENT,"
          ++ "area INT,"
          ++ "\"objectType\" INT,"
          ++ "\"buildingType\" INT,"
          ++ "\"addressId\" INTEGER,"
          ++ "\"ot\" INTEGER DEFAULT 5, "
          ++ "FOREIGN KEY (\"addressId\") REFERENCES addresses(id)"
          ++ ");"
    )

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

  -- Вставка данных в таблицу addresses (добавлены новые адреса)
  execute_
    sqlConnection
    ( fromString $
        "INSERT INTO addresses (state, city, district, \"postalCode\", \"streetName\", \"houseNumber\", entrance, \"doorNumber\") VALUES "
          ++ "('Томская область', 'Томск', 'Кировский', '100001', 'Нахимова', '12', 1, 101),"
          ++ "('Томская область', 'Томск', 'Кировский', '100002', 'Нахимова', '13', 1, 102),"
          ++ "('Томская область', 'Томск', 'Советский', '100010', 'Елизаровых', '21', 5, 110),"
          ++ "('Томская область', 'Томск', 'Советский', '100011', 'Балтийская', '22', 6, 111),"
          ++ "('Томская область', 'Томск', 'Советский', '100012', 'Елизаровых', '23', 6, 112),"
          ++ "('Томская область', 'Томск', 'Советский', '100016', 'Балтийская', '27', 8, 116),"
          ++ "('Томская область', 'Томск', 'Советский', '100017', 'Комсомольский проспект', '28', 9, 117),"
          ++ "('Томская область', 'Томск', 'Советский', '100021', 'Комсомольский проспект', '32', 11, 121),"
          ++ "('Томская область', 'Томск', 'Советский', '100022', 'Комсомольский проспект', '33', 12, 122),"
          ++ "('Томская область', 'Томск', 'Октябрьский', '100006', 'Иркутский тракт', '17', 3, 106),"
          ++ "('Томская область', 'Томск', 'Октябрьский', '100007', 'Иркутский тракт', '18', 4, 107),"
          ++ "('Томская область', 'Томск', 'Октябрьский', '100008', 'Иркутский тракт', '19', 4, 108),"
          ++ "('Томская область', 'Томск', 'Октябрьский', '100009', 'Иркутский тракт', '20', 5, 109),"
          ++ "('Томская область', 'Томск', 'Октябрьский', '100013', 'Суворова', '24', 7, 113),"
          ++ "('Томская область', 'Томск', 'Октябрьский', '100014', 'Торговая', '25', 7, 114),"
          ++ "('Томская область', 'Томск', 'Октябрьский', '100015', 'Ивановского', '26', 8, 115),"
          ++ "('Томская область', 'Томск', 'Ленинский', '100003', 'Большая подгорная', '14', 2, 103),"
          ++ "('Томская область', 'Томск', 'Ленинский', '100004', 'Первомайская', '15', 2, 104),"
          ++ "('Томская область', 'Томск', 'Ленинский', '100005', 'Donskoy pereulok', '16', 3, 105),"
          ++ "('Томская область', 'Томск', 'Ленинский', '100019', 'Первомайская', '30', 10, 119),"
          ++ "('Томская область', 'Томск', 'Ленинский', '100018', 'Проспект мира', '29', 9, 118),"
          ++ "('Томская область', 'Томск', 'Ленинский', '100020', 'Проспект мира', '31', 10, 120),"
          ++ "('Томская область', 'Томск', 'Ленинский', '100023', 'Тихий переулок', '34', 13, 123), "
          ++ "('Томская область', 'Томск', 'Ленинский', '100024', 'Проспект Ленина', '35', 14, 124);"
    )

  -- Вставка данных в таблицу flats (добавлены новые записи)
  execute_
    sqlConnection
    ( fromString $
        "INSERT INTO flats (id, area, \"roomCount\", \"addressId\", floor, \"floorsCount\", \"balconyArea\") VALUES "
          ++ "(1, 45, 2, 9, 6, 2, 10), "
          ++ "(2, 34, 1, 10, 2, 1, 0), "
          ++ "(3, 60, 3, 1, 5, 10, 15), "
          ++ "(4, 55, 2, 2, 3, 5, 8), "
          ++ "(6, 50, 3, 18, 4, 2, 15);"
    )

  -- Вставка данных в таблицу houses (добавлены новые записи)
  execute_
    sqlConnection
    ( fromString $
        "INSERT INTO houses (id, area, \"areaType\", \"addressId\", \"roomCount\", \"floorsCount\", \"basementArea\") VALUES "
          ++ "(1, 1320, 2, 7, 6, 2, 0), "
          ++ "(2, 1520, 1, 8, 2, 1, 0), "
          ++ "(3, 1600, 1, 11, 4, 2, 500), "
          ++ "(4, 1800, 2, 12, 5, 3, 600), "
          ++ "(7, 2000, 1, 21, 3, 2, 400);"
    )

  -- Вставка данных в таблицу landPlot (добавлены новые записи)
  execute_
    sqlConnection
    ( fromString $
        "INSERT INTO landPlot (id, area, \"landCategory\", \"addressId\") VALUES "
          ++ "(1, 3500, 1, 5), "
          ++ "(2, 1900, 3, 6), "
          ++ "(4, 4000, 2, 13), "
          ++ "(8, 5000, 3, 14);"
    )

  -- Вставка данных в таблицу garages (добавлены новые записи)
  execute_
    sqlConnection
    ( fromString $
        "INSERT INTO garages (id, area, security, \"addressId\") VALUES "
          ++ "(1, 1100, TRUE, 3), "
          ++ "(2, 900, FALSE, 4), "
          ++ "(5, 1000, TRUE, 17);"
    )

  -- Вставка данных в таблицу commercialRealEstates (добавлены новые записи)
  execute_
    sqlConnection
    ( fromString $
        "INSERT INTO commercialRealEstates (id, area, \"objectType\", \"buildingType\", \"addressId\") VALUES "
          ++ "(9, 5000, 1, 2, 19), "
          ++ "(10, 6000, 2, 3, 20);"
    )

  -- Вставка данных в таблицу wallets
  execute_
    sqlConnection
    ( fromString $
        "INSERT INTO wallets (id, balance) VALUES "
          ++ "(1, 100100), "
          ++ "(2, 400000), "
          ++ "(3, 400200), "
          ++ "(4, 100200), "
          ++ "(5, 100000), "
          ++ "(6, 100000);"
    )

  -- Вставка данных в таблицу users
  execute_
    sqlConnection
    ( fromString $
        "INSERT INTO users (id, name, surname, email, password, wallet) VALUES "
          ++ "(1, 'Egor', 'Zhvakin', 'egor.zhvakin@mail.ru', 'pass', 1), "
          ++ "(2, 'Stanislav', 'Hohlov', 'ssstttaaasssiiikk@mail.ru', 'pass', 2), "
          ++ "(3, 'Bogdan', 'Eremin', 'bodya43@mail.ru', 'pass', 3), "
          ++ "(4, 'Miron', 'Neborski', 'miroooosha@mail.ru', 'pass', 4), "
          ++ "(5, 'Vladislav', 'Verkholansev', 'verxolancev@mail.ru', 'pass', 5), "
          ++ "(6, 'Daniil', 'Ivanichev', 'yaneidd@mail.ru', 'pass', 6);"
    )

  -- Вставка данных в таблицу ads с корректными objectType и описаниями
  execute_
    sqlConnection
    ( fromString $
        "INSERT INTO ads (id, seller, \"objectId\", \"objectType\", cost, description) VALUES "
          ++ "(1, 1, 1, 1, 10000, 'Продается уютная квартира в центре города.'), "
          ++ "(2, 2, 2, 1, 15000, 'Большая квартира с двумя балконами.'), "
          ++ "(3, 3, 3, 2, 20000, 'Современный дом с садом и гаражом.'), "
          ++ "(4, 4, 4, 3, 25000, 'Земельный участок под строительство дома.'), "
          ++ "(5, 1, 5, 4, 30000, 'Гараж с автоматической дверью.'), "
          ++ "(6, 2, 6, 1, 12000, 'Квартира рядом с парком.'), "
          ++ "(7, 3, 7, 2, 18000, 'Дом с бассейном и сауной.'), "
          ++ "(8, 4, 8, 3, 22000, 'Земельный участок в экологически чистом районе.'), "
          ++ "(9, 1, 9, 5, 35000, 'Коммерческая недвижимость в центре города.'), "
          ++ "(10, 4, 10, 5, 40000, 'Офисное помещение с парковкой.');"
    )

  -- Вставка данных в таблицу deals
  execute_
    sqlConnection
    ( fromString $
        "INSERT INTO deals (id, buyer, \"adId\", \"dealType\", status, date, \"finalCost\") VALUES "
          ++ "(1, 2, 5, '1', 'new', '2024-08-16', 10000), "
          ++ "(2, 1, 6, '1', 'close', '2024-08-16', 20000), "
          ++ "(3, 4, 7, '1', 'reject', '2024-08-16', 100000000), "
          ++ "(4, 1, 8, '1', 'new', '2024-08-16', 17500000), "
          ++ "(5, 1, 1, '1', 'close', '2024-08-16', 67000);"
    )

  close sqlConnection

getLastId :: Connection -> IO Integer
getLastId conn = do
  [Only lastId] <- query_ conn "SELECT last_insert_rowid();" :: IO [Only Integer]
  return lastId
