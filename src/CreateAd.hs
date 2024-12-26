{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module CreateAd where

import CreateFlat (createFlat)
import SQLplotter (getLastId, getUserSession)
import Database.SQLite.Simple

createAd :: IO ()
createAd = do
  dataBase <- open "local.db"
  id <- findOrCreateAddress dataBase
  router dataBase id
  close dataBase

router :: Connection -> Integer -> IO ()
router dataBase address = do
  owner <- getUserSession
  putStrLn "Создание объявления"
  putStrLn "Выберите тип объекта:"
  putStrLn "1. Квартира"
  putStrLn "2. Дом"
  putStrLn "3. Земельный участок"
  putStrLn "4. Гараж"
  putStrLn "5. Коммерческая недвижимость"
  putStrLn "Введите выбранный пункт меню:"

  choice <- getLine
  case choice of
    "1" -> createFlat dataBase owner address
    "2" -> putStrLn "Вы выбрали дом"
    "3" -> putStrLn "Вы выбрали земельный участок"
    "4" -> putStrLn "Вы выбрали гараж"
    "5" -> putStrLn "Вы выбрали коммерческую недвижимость"
    _ -> putStrLn "Неверный выбор, попробуйте снова."

findOrCreateAddress :: Connection -> IO Integer
findOrCreateAddress dataBase = do
  putStrLn "--------- Добавим адрес --------"
  state <- getString "Введите регион:"
  city <- getString "Введите город:"
  district <- getString "Введите район:"
  postalCode <- getString "Введите почтовый индекс:"
  streetName <- getString "Введите улицу:"
  houseNumber <- getString "Введите номер дома:"
  entranceStr <- getString "Введите подъезд (оставьте пустым если нет):"
  let entrance = if null entranceStr then Nothing else Just (read entranceStr :: Integer)
  putStrLn "Номер квартиры (оставьте пустым если нет):"
  doorNumberStr <- getLine
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
