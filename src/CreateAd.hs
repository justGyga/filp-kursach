{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module CreateAd where

import Commons (getFloat, getString)
import CreateAddress (findOrCreateAddress)
import CreateFlat (createFlat)
import CreateHouse (createHouse)
import CreateLandPlot (createLandPlot)
import Database.SQLite.Simple
import SQLplotter (getLastId, getUserSession)

createAd :: IO ()
createAd = do
  dataBase <- open "local.db"
  router dataBase
  close dataBase

router :: Connection -> IO ()
router dataBase = do
  owner <- getUserSession
  putStrLn "--------- Создание объявления --------"
  putStrLn "Выберите тип объекта:"
  putStrLn "1. Квартира"
  putStrLn "2. Дом"
  putStrLn "3. Земельный участок"
  putStrLn "4. Гараж"
  putStrLn "5. Коммерческая недвижимость"
  putStrLn "Введите выбранный пункт меню:"

  choice <- getLine
  case choice of
    "1" -> do
      addressId <- findOrCreateAddress dataBase
      objectId <- createFlat dataBase addressId
      createAdService dataBase 1 owner objectId
    "2" -> do
      addressId <- findOrCreateAddress dataBase
      objectId <- createHouse dataBase addressId
      createAdService dataBase 2 owner objectId
    "3" -> do
      addressId <- findOrCreateAddress dataBase
      objectId <- createLandPlot dataBase addressId
      createAdService dataBase 3 owner objectId
    "4" -> putStrLn "Вы выбрали гараж"
    "5" -> putStrLn "Вы выбрали коммерческую недвижимость"
    _ -> putStrLn "Неверный выбор, попробуйте снова."

createAdService :: Connection -> Integer -> Integer -> Integer -> IO ()
createAdService dataBase objectType ownerId objectId = do
  cost <- getFloat "Введите стоимость квартиры (в рублях):"
  description <- getString "Введите описание квартиры:" False

  executeNamed
    dataBase
    "INSERT INTO ads (seller, \"objectId\", \"objectType\", cost, description) VALUES (:seller, :objectId, :objectType, :cost, :description)"
    [ ":seller" := (ownerId :: Integer),
      ":objectId" := (objectId :: Integer),
      ":objectType" := (objectType :: Integer),
      ":cost" := (cost :: Float),
      ":description" := (description :: String)
    ]

  adId <- getLastId dataBase

  putStrLn "\nОбъявление о продаже квартиры успешно создано! ID объявления: "
  putStrLn $ show adId
