{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module CreateHouse where

import Commons (getInteger)
import Database.SQLite.Simple
import Enums (getAreaType)
import SQLplotter (getLastId)

createHouse :: Connection -> Integer -> IO Integer
createHouse dataBase addressId = do
  area <- getInteger "Введите площадь дома (в кв.м.):" 1
  areaType <- getAreaType
  roomCount <- getInteger "Введите количество комнат:" 1
  floorsCount <- getInteger "Введите количество этажей:" 1
  basementArea <- getInteger "Введите площадь подвала (в кв.м.):" 0

  execute
    dataBase
    "INSERT INTO houses (area, \"areaType\", \"addressId\", \"roomCount\", \"floorsCount\", \"basementArea\") VALUES (?, ?, ?, ?, ?, ?)"
    (area, areaType, addressId, roomCount, floorsCount, basementArea)

  getLastId dataBase
