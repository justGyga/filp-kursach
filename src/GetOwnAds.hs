{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module GetOwnAds where

import Data.String (fromString)
import Database.SQLite.Simple
import Enums (allAdObjectTypes)
import SQLplotter (getUserSession)
import DataTypes
import GetAdById (getAdById)
import DeleteAd
import EditAd

getOwnAds :: IO ()
getOwnAds = do
  dataBase <- open "local.db"
  selfId <- getUserSession
  if selfId == -1
    then putStrLn "Пользователь не авторизован"
    else do
      ids <- getOwnAdsService dataBase selfId
      choice <- selectAdId ids
      case choice of
        Just adId -> do
          getAdById dataBase adId
          putStrLn "----- Выберите действие -----"
          putStrLn "1) Отредактировать объявление"
          putStrLn "2) Удалить объявление"
          putStrLn "Покинуть"
          
          action <- getLine
          case action of
            "1" -> do
              editAd dataBase adId
            "2" -> do
              deleteAd dataBase adId
            _ -> do
              putStrLn "Функционал не реализован"
        Nothing -> return ()
      return ()
  close dataBase


selectAdId :: [Integer] -> IO (Maybe Integer)
selectAdId [] = return Nothing
selectAdId ids = do
  putStrLn "\nВыберите номер объявления или введите 'q' для выхода:"
  input <- getLine
  case input of
    "q" -> return Nothing 
    _ -> case reads input of
          [(n, "")] -> if n `elem` ids
                        then return $ Just n
                        else do
                          putStrLn "Неверный номер объявления. Попробуйте снова."
                          selectAdId ids
          _ -> do
            putStrLn "Неверный ввод. Попробуйте снова."
            selectAdId ids


getOwnAdsService :: Connection -> Integer -> IO [Integer]
getOwnAdsService dataBase selfId = do
  let baseQuery =
        fromString $
          "SELECT "
            ++ "ads.id, "
            ++ "ads.seller, "
            ++ "ads.\"objectId\", "
            ++ "ads.\"objectType\", "
            ++ "ads.cost, "
            ++ "ads.description, "
            ++ "addresses.id AS addressId, "
            ++ "addresses.state, "
            ++ "addresses.city, "
            ++ "addresses.district, "
            ++ "addresses.\"postalCode\", "
            ++ "addresses.\"streetName\", "
            ++ "addresses.\"houseNumber\", "
            ++ "CAST(NULLIF(addresses.entrance, '') AS INTEGER) AS entrance, "
            ++ "CAST(NULLIF(addresses.\"doorNumber\", '') AS INTEGER) AS doorNumber, "
            ++ "COALESCE(CAST(objs.area AS INTEGER), 0) AS objectArea "
            ++ "FROM ads "
            ++ "INNER JOIN ( "
            ++ "  SELECT id, area, \"addressId\", ot "
            ++ "  FROM flats "
            ++ "  UNION "
            ++ "  SELECT id, area, \"addressId\", ot "
            ++ "  FROM houses "
            ++ "  UNION "
            ++ "  SELECT id, area, \"addressId\", ot "
            ++ "  FROM \"landPlot\" "
            ++ "  UNION "
            ++ "  SELECT id, area, \"addressId\", ot "
            ++ "  FROM garages "
            ++ "  UNION "
            ++ "  SELECT id, area, \"addressId\", ot "
            ++ "  FROM \"commercialRealEstates\" "
            ++ ") AS objs ON ads.\"objectId\" = objs.id AND ads.\"objectType\" = objs.ot "
            ++ "JOIN addresses ON objs.\"addressId\" = addresses.id "
            ++ "WHERE seller = ? ORDER BY ads.id ASC"

  ads <- query dataBase baseQuery (Only selfId) :: IO [RawAdData]
  if null ads
    then do
      putStrLn "У вас нет активных объявлений."
      return []
    else do
      putStrLn "\n----- Ваши Объявления -----\n"
      mapM_ printAdWithAddress ads
      return $ map rawObjectId ads

printAdWithAddress :: RawAdData -> IO ()
printAdWithAddress ad = do
  putStrLn $ "ID Объявления:\t" ++ show (rawAdId ad)
  putStrLn $ "Тип Объекта:\t" ++ showObjectType (rawObjectType ad)
  putStrLn $ "ID Объекта:\t" ++ show (rawObjectId ad)
  putStrLn $ "Стоимость:\t" ++ show (rawCost ad) ++ " RUB"
  putStrLn $ "Описание:\t" ++ rawDescription ad
  putStrLn "----- Адрес Объекта -----"
  putStrLn $ "Регион:\t\t" ++ rawState ad
  putStrLn $ "Город:\t\t" ++ rawCity ad
  putStrLn $ "Район:\t\t" ++ rawDistrict ad
  putStrLn $ "Почтовый Код:\t" ++ rawPostalCode ad
  putStrLn $ "Улица:\t\t" ++ rawStreetName ad
  putStrLn $ "Номер Дома:\t" ++ rawHouseNumber ad
  case rawEntrance ad of
    Just ent -> putStrLn $ "Подъезд:\t" ++ show ent
    Nothing -> putStrLn "Подъезд:\t\tНе указано"
  case rawDoorNumber ad of
    Just dn -> putStrLn $ "Номер Двери:\t" ++ show dn
    Nothing -> putStrLn "Номер Двери:\t\tНе указан"
  putStrLn $ "Площадь:\t" ++ show (rawObjectArea ad) ++ " кв.м."
  putStrLn "-----------------------------------"

showObjectType :: Integer -> String
showObjectType n = case lookup n allAdObjectTypes of
  Just name -> name
  Nothing -> "Неизвестный тип объекта"
