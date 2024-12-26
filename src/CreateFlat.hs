{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module CreateFlat where

import Commons (getInteger)
import Database.SQLite.Simple
import SQLplotter (getLastId)

createFlat :: Connection -> Integer -> IO Integer
createFlat dataBase addressId = do
  area <- getInteger "Введите площадь квартиры (в кв.м.):" 1
  roomCount <- getInteger "Введите количество комнат:" 1
  floorNumber <- getInteger "Введите этаж:" 1
  floorsCount <- getInteger "Введите количество этажей:" 1
  balconyArea <- getInteger "Введите площадь балкона (в кв.м.):" 0

  execute
    dataBase
    "INSERT INTO flats (area, \"roomCount\", \"addressId\", floor, \"floorsCount\", \"balconyArea\") VALUES (?, ?, ?, ?, ?, ?)"
    (area, roomCount, addressId, floorNumber, floorsCount, balconyArea)

  getLastId dataBase
