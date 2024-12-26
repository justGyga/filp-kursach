{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module CreateAddress where

import Commons (getString)
import Database.SQLite.Simple
import SQLplotter (getLastId)

findOrCreateAddress :: Connection -> IO Integer
findOrCreateAddress dataBase = do
  putStrLn "--------- Добавим адрес --------"
  state <- getString "Введите регион:" False
  city <- getString "Введите город:" False
  district <- getString "Введите район:" False
  postalCode <- getString "Введите почтовый индекс:" False
  streetName <- getString "Введите улицу:" False
  houseNumber <- getString "Введите номер дома:" False
  entranceStr <- getString "Введите подъезд (оставьте пустым если нет):" True
  let entrance = if null entranceStr then Nothing else Just (read entranceStr :: Integer)
  doorNumberStr <- getString "Номер квартиры (оставьте пустым если нет):" True
  let doorNumber = if null doorNumberStr then Nothing else Just (read doorNumberStr :: Integer)

  addresses <-
    queryNamed
      dataBase
      "SELECT id FROM addresses WHERE state = :state AND city = :city AND district = :district AND postalCode = :postalCode AND streetName = :streetName AND houseNumber = :houseNumber AND entrance = :entrance AND doorNumber = :doorNumber"
      [ ":state" := state,
        ":city" := city,
        ":district" := district,
        ":postalCode" := postalCode,
        ":streetName" := streetName,
        ":houseNumber" := houseNumber,
        ":entrance" := entrance,
        ":doorNumber" := doorNumber
      ] ::
      IO [Only Integer]

  case addresses of
    [Only existingId] -> return existingId
    _ -> do
      execute
        dataBase
        "INSERT INTO addresses (state, city, district, postalCode, streetName, houseNumber, entrance, doorNumber) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        (state, city, district, postalCode, streetName, houseNumber, entrance, doorNumber)
      getLastId dataBase