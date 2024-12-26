module GetOwnAds where

import Data.String (fromString)
import Database.SQLite.Simple
  ( Connection,
    FromRow (..),
    Only (Only),
    close,
    field,
    open,
    query,
  )
import Enums (allAdObjectTypes)
import SQLplotter (getUserSession)
import DataTypes (RawAdData(..), rawAdId, rawObjectId, rawObjectType, rawSeller, rawCost, rawDescription, 
                 rawAddressId, rawState, rawCity, rawDistrict, rawPostalCode, rawStreetName, 
                 rawHouseNumber, rawEntrance, rawDoorNumber, rawObjectArea)

getOwnAds :: IO ()
getOwnAds = do
  dataBase <- open "local.db"
  selfId <- getUserSession
  if selfId == -1
    then putStrLn "Пользователь не авторизован"
    else do
      getOwnAdsService dataBase selfId
  close dataBase

getOwnAdsService :: Connection -> Integer -> IO ()
getOwnAdsService dataBase selfId = do
  let baseQuery =
        fromString $
          "SELECT "
            ++ "ads.id, "
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
    then putStrLn "У вас нет активных объявлений."
    else do
      putStrLn "\n----- Ваши Объявления -----\n"
      mapM_ printAdWithAddress ads

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
