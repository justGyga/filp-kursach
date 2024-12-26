{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module CreateLandPlot where

import Commons (getInteger, getString)
import CreateAddress (findOrCreateAddress)
import Database.SQLite.Simple
import Enums (getLandCategory)
import SQLplotter (getLastId, getUserSession)

createLandPlot :: Connection -> Integer -> IO Integer
createLandPlot dataBase addressId = do
  area <- getInteger "Введите площадь земельного участка (в кв.м.):" 1
  landCategory <- getLandCategory

  execute
    dataBase
    "INSERT INTO landPlot (area, \"landCategory\", \"addressId\") VALUES (?, ?, ?)"
    (area, landCategory, addressId)

  getLastId dataBase
