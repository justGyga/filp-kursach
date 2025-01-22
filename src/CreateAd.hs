{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module CreateAd where

import Commons (getFloat, getString, clearCLI)
import CreateAddress (findOrCreateAddress)
import CreateFlat (createFlat)
import CreateHouse (createHouse)
import CreateLandPlot (createLandPlot)
import CreateGarage (createGarage)
import CreateCommercialRealEstate (createCommercialRealEstate)
import Database.SQLite.Simple
import SQLplotter (getLastId, getUserSession)

createAd :: IO ()
createAd = do
  dataBase <- open "local.db"
  router dataBase
  close dataBase
      
getDealType :: IO Integer
getDealType = do
  putStrLn "Выберите тип сделки:"
  putStrLn "1) Аренда"
  putStrLn "2) Продажа"
  input <- getLine
  case reads input :: [(Integer, String)] of
    [(dealType, "")] | dealType `elem` [1,2] -> return dealType
    _ -> do
      putStrLn "Некорректное значение. Пожалуйста, введите 1 или 2."
      getDealType

router :: Connection -> IO ()
router dataBase = do
  owner <- getUserSession
  dealType <- getDealType
  putStrLn "--------- Создание объявления --------"
  putStrLn "Выберите тип объекта:"
  putStrLn "1. Квартира"
  putStrLn "2. Дом"
  putStrLn "3. Земельный участок"
  putStrLn "4. Гараж"
  putStrLn "5. Коммерческая недвижимость"
  putStrLn "6. Выйти"
  putStrLn "Введите выбранный пункт меню:"

  choice <- getLine
  case choice of
    "1" -> do
      addressId <- findOrCreateAddress dataBase
      objectId <- createFlat dataBase addressId
      createAdService dataBase 1 owner objectId dealType
    "2" -> do
      addressId <- findOrCreateAddress dataBase
      objectId <- createHouse dataBase addressId
      createAdService dataBase 2 owner objectId dealType
    "3" -> do
      addressId <- findOrCreateAddress dataBase
      objectId <- createLandPlot dataBase addressId
      createAdService dataBase 3 owner objectId dealType
    "4" -> do
      addressId <- findOrCreateAddress dataBase
      objectId <- createGarage dataBase addressId
      createAdService dataBase 4 owner objectId dealType
    "5" -> do
      addressId <- findOrCreateAddress dataBase
      objectId <- createCommercialRealEstate dataBase addressId
      createAdService dataBase 5 owner objectId dealType
    "6" -> putStrLn "Возвращение в главное меню..."
    _ -> putStrLn "Неверный выбор, попробуйте снова."

createAdService :: Connection -> Integer -> Integer -> Integer -> Integer -> IO ()
createAdService dataBase objectType ownerId objectId dealType = do
  cost <- getFloat "Введите стоимость объекта (в рублях):"
  description <- getString "Введите описание объекта:" False

  executeNamed
    dataBase
    "INSERT INTO ads (seller, \"objectId\", \"objectType\", cost, description, \"dealType\") VALUES (:seller, :objectId, :objectType, :cost, :description, :dealType)"
    [ ":seller" := (ownerId :: Integer),
      ":objectId" := (objectId :: Integer),
      ":objectType" := (objectType :: Integer),
      ":cost" := (cost :: Float),
      ":description" := (description :: String),
      ":dealType" := (dealType :: Integer)
    ]

  adId <- getLastId dataBase

  -- clearCLI

  putStrLn "\nОбъявление о продаже квартиры успешно создано! ID объявления: "
  putStrLn $ show adId
