{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module CreateFlat where

import Commons (getFloat, getInteger, getString)
import Database.SQLite.Simple
import SQLplotter (getLastId)
createFlat :: Connection -> Integer -> Integer -> IO ()
createFlat dataBase ownerId addressId = do
  area <- getInteger "Введите площадь квартиры (в кв.м.):"
  roomCount <- getInteger "Введите количество комнат:"
  floor <- getInteger "Введите этаж:"
  floorsCount <- getInteger "Введите количество этажей:"
  balconyArea <- getInteger "Введите площадь балкона (в кв.м.):"
  cost <- getFloat "Введите стоимость квартиры (в рублях):"
  description <- getString "Введите описание квартиры:"

  execute
    dataBase
    "INSERT INTO flats (area, \"roomCount\", \"addressId\", floor, \"floorsCount\", \"balconyArea\") VALUES (?, ?, ?, ?, ?, ?)"
    (area, roomCount, addressId, floor, floorsCount, balconyArea)

  flatId <- getLastId dataBase

  executeNamed
    dataBase
    "INSERT INTO ads (seller, \"objectId\", \"objectType\", cost, description) VALUES (:seller, :objectId, :objectType, :cost, :description)"
    [ ":seller" := (ownerId :: Integer),
      ":objectId" := (flatId :: Integer),
      ":objectType" := (1 :: Integer),
      ":cost" := (cost :: Float),
      ":description" := (description :: String)
    ]

  adId <- getLastId dataBase

  putStrLn "\nОбъявление о продаже квартиры успешно создано! ID объявления: "
  putStrLn $ show adId