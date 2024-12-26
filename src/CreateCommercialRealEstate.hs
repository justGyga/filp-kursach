{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module CreateCommercialRealEstate where

import Commons (getInteger)
import Database.SQLite.Simple
import Enums (getCommercialObjectType)
import SQLplotter (getLastId)

createCommercialRealEstate :: Connection -> Integer -> IO Integer
createCommercialRealEstate dataBase addressId = do
  area <- getInteger "Введите площадь коммерческой недвижимости (в кв.м.):" 1
  objectType <- getCommercialObjectType

  execute dataBase "INSERT INTO \"commercialRealEstates\" (area, \"objectType\", \"addressId\") VALUES (?, ?, ?)" (area, objectType, addressId)
  getLastId dataBase
