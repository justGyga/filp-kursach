{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module CreateGarage where

import           Commons                (getBoolean, getInteger)
import           Database.SQLite.Simple
import           SQLplotter             (getLastId)

createGarage :: Connection -> Integer -> IO Integer
createGarage dataBase addressId = do
  area <- getInteger "Введите площадь гаража (в кв.м.):" 1
  security <- getBoolean "Есть ли охрана?"

  execute dataBase "INSERT INTO garages (area, security, \"addressId\") VALUES (?, ?, ?)" (area, security, addressId)
  getLastId dataBase

