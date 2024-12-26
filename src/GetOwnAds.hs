module GetOwnAds where

import Data.String (fromString)
import Database.SQLite.Simple
    ( Connection, Only(Only), close, open, query, field, FromRow(..) )
import SQLplotter (getUserSession)

data RawAdData = RawAdData
  { rawAdId :: Integer,
    rawObjectId :: Integer,
    rawObjectType :: Integer,
    rawCost :: Float,
    rawDescription :: String,
    rawAddressId :: Integer,
    rawState :: String,
    rawCity :: String,
    rawDistrict :: String,
    rawPostalCode :: String,
    rawStreetName :: String,
    rawHouseNumber :: String,
    rawEntrance :: Maybe Integer,
    rawDoorNumber :: Maybe Integer,
    rawObjectArea :: Int
  }
  deriving (Show)

instance FromRow RawAdData where
  fromRow =
    RawAdData
      <$> field -- id
      <*> field -- objectId
      <*> field -- objectType
      <*> field -- cost
      <*> field -- description
      <*> field -- addressId
      <*> field -- state
      <*> field -- city
      <*> field -- district
      <*> field -- postalCode
      <*> field -- streetName
      <*> field -- houseNumber
      <*> field -- entrance
      <*> field -- doorNumber
      <*> field -- objectArea

getOwnAds :: IO ()
getOwnAds = do
  dataBase <- open "local.db"
  id <- getUserSession
  if id == -1
    then putStrLn "Пользователь не авторизован"
    else do
      getOwnAdsService dataBase id
  close dataBase

getOwnAdsService :: Connection -> Integer -> IO ()
getOwnAdsService dataBase id = do
  let baseQuery = fromString $
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

  ads <- query dataBase baseQuery (Only id) :: IO [RawAdData]
  if null ads
    then putStrLn "У вас нет активных объявлений."
    else do
      putStrLn "\n----- Ваши Объявления -----\n"
      mapM_ printAdWithAddress ads
      putStrLn "\n----------------------------\n"

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
showObjectType n = case lookup n allObjectTypes of
  Just name -> name
  Nothing -> "Неизвестный тип объекта"

-- Добавим список всех типов объектов
allObjectTypes :: [(Integer, String)]
allObjectTypes =
  [ (1, "Квартира"),
    (2, "Дом"),
    (3, "Земельный участок"),
    (4, "Гараж"),
    (5, "Коммерческая недвижимость")
  ]
