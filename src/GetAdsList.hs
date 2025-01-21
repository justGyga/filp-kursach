{-# LANGUAGE OverloadedStrings #-}

module GetAdsList where

import Commons (clearCLI)
import Data.List (find)
import Data.String (fromString)
import DataTypes
import Database.SQLite.Simple
import Enums (allAdObjectTypes)
import Filters (districtFilter, maxAreaFilter, maxCostFilter, minAreaFilter, minimalCostFilter, objectTypeFilter, dealTypeFilter)
import GetAdById (getAdById)
import SQLplotter (getUserSession)

data Address = Address
  { addressId :: Integer,
    state :: String,
    city :: String,
    district :: String,
    postalCode :: String,
    streetName :: String,
    houseNumber :: String,
    entrance :: Maybe Integer,
    doorNumber :: Maybe Integer
  }
  deriving (Show)

instance FromRow Address where
  fromRow =
    Address
      <$> field -- addressId
      <*> field -- state
      <*> field -- city
      <*> field -- district
      <*> field -- postalCode
      <*> field -- streetName
      <*> field -- houseNumber (теперь String)
      <*> field -- entrance
      <*> field -- doorNumber

instance ToRow Address where
  toRow (Address addressId state city district postalCode streetName houseNumber entrance doorNumber) =
    toRow (addressId, state, city, district, postalCode, streetName, houseNumber, entrance, doorNumber)

data Ad = Ad
  { adId :: Integer,
    seller :: Integer,
    objectId :: Integer,
    objectType :: Integer,
    cost :: Float,
    description :: String,
    dealType :: Integer
  }
  deriving (Show)

instance FromRow Ad where
  fromRow = Ad <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Ad where
  toRow (Ad adId seller objectId objectType cost description dealType) =
    toRow (adId, seller, objectId, objectType, cost, description, dealType)

data AdWithAddress = AdWithAddress
  { ad :: Ad,
    address :: Address,
    objectTypeWA :: Integer,
    area :: Int
  }
  deriving (Show)

-- | Экземпляр `FromRow` для `AdWithAddress`, позволяет считывать записи из базы данных.
instance FromRow AdWithAddress where
  fromRow = do
    -- Чтение полей для Ad
    adId <- field
    seller <- field
    objectId <- field
    objectType <- field
    cost <- field
    description <- field
    dealType <- field

    -- Чтение полей для Address
    addressId <- field
    state <- field
    city <- field
    district <- field
    postalCode <- field
    streetName <- field
    houseNumber <- field
    entrance <- field
    doorNumber <- field

    -- Чтение дополнительных полей
    objectTypeWA <- field
    area <- field

    let ad = Ad adId seller objectId objectType cost description dealType
    let address = Address addressId state city district postalCode streetName houseNumber entrance doorNumber

    return $ AdWithAddress ad address objectTypeWA area

convertRawToAdWithAddress :: RawAdData -> AdWithAddress
convertRawToAdWithAddress raw =
  let ad =
        Ad
          { adId = rawAdId raw,
            seller = rawSeller raw,
            objectId = rawObjectId raw,
            objectType = rawObjectType raw,
            cost = rawCost raw,
            description = rawDescription raw,
            dealType = rawDealType raw
          }
      address =
        Address
          { addressId = rawAddressId raw,
            state = rawState raw,
            city = rawCity raw,
            district = rawDistrict raw,
            postalCode = rawPostalCode raw,
            streetName = rawStreetName raw,
            houseNumber = rawHouseNumber raw,
            entrance = rawEntrance raw,
            doorNumber = rawDoorNumber raw
          }
   in AdWithAddress ad address (rawObjectType raw) (rawObjectArea raw)

-- | Вспомогательная функция для форматирования и вывода объявления вместе с адресом и площадью.
printAdWithAddress :: AdWithAddress -> IO ()
printAdWithAddress (AdWithAddress ad addr ot area) = do
  putStrLn $ "+- ID Объявления:\t" ++ show (adId ad)
  putStrLn $ "|  ID Продавца:\t" ++ show (seller ad)
  putStrLn $ "|  Тип Объекта:\t" ++ showObjectType (objectType ad)
  putStrLn $ "|  ID Объекта:\t" ++ show (objectId ad)
  putStrLn $ "|  Тип сделки:\t" ++ showDealType (dealType ad)
  putStrLn $ "|  Стоимость:\t" ++ show (cost ad) ++ " RUB"
  putStrLn $ "|  Описание:\t" ++ description ad
  putStrLn "+- Адрес Объекта -----"
  putStrLn $ "|  Регион:\t\t" ++ state addr
  putStrLn $ "|  Город:\t\t" ++ city addr
  putStrLn $ "|  Район:\t\t" ++ district addr
  putStrLn $ "|  Почтовый Код:\t" ++ postalCode addr
  putStrLn $ "|  Улица:\t\t" ++ streetName addr
  putStrLn $ "|  Номер Дома:\t" ++ houseNumber addr
  case entrance addr of
    Just ent -> putStrLn $ "|  Подъезд:\t" ++ show ent
    Nothing -> putStrLn $ "|  Подъезд:\t\tНе указано"
  case doorNumber addr of
    Just dn -> putStrLn $ "|  Номер Двери:\t" ++ show dn
    Nothing -> putStrLn $ "|  Номер Двери:\t\tНе указан"
  putStrLn $ "|  Площадь:\t" ++ show area ++ " кв.м."
  putStrLn "+--------------------------------"

showDealType :: Integer -> String
showDealType n = case n of
  1 -> "Аренда"
  2 -> "Продажа" 
  _ -> "Неясный тип (" ++ show n ++ ")"


showObjectType :: Integer -> String
showObjectType n = case lookup n allAdObjectTypes of
  Just name -> name
  Nothing -> "Неизвестный тип объекта"

-- | Новая функция для фильтрации доступных объявлений
filterAvailableAds :: IO ()
filterAvailableAds = do
  dataBase <- open "local.db"

  -- Сбор фильтров от пользователя
  putStrLn "\n----- Фильтрация Объявлений -----\n"

  districtFilterQuery <- districtFilter dataBase
  objectTypeFilterQuery <- objectTypeFilter
  minimalCostFilterQuery <- minimalCostFilter
  maxCostFilterQuery <- maxCostFilter
  minAreaFilterQuery <- minAreaFilter
  maxAreaFilterQuery <- maxAreaFilter
  dealTypeQuery <- dealTypeFilter
  clearCLI

  currentUserId <- getUserSession
  let sellerFilter = "deals.status = 'new' AND ads.seller != " ++ show currentUserId

  let baseQuery =
        "SELECT "
          ++ "ads.id, "
          ++ "ads.seller, "
          ++ "ads.\"objectId\", "
          ++ "ads.\"objectType\", "
          ++ "ads.cost, "
          ++ "ads.description, "
          ++ "ads.\"dealType\", "
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
          ++ "INNER JOIN deals ON ads.id = deals.\"adId\""
          ++ "JOIN addresses ON objs.\"addressId\" = addresses.id "
          ++ "WHERE "
          ++ sellerFilter -- Начало условий WHERE

  -- Построение условий WHERE и списка параметров
  let conditions = districtFilterQuery ++ objectTypeFilterQuery ++ minimalCostFilterQuery ++ maxCostFilterQuery ++ minAreaFilterQuery ++ maxAreaFilterQuery ++ dealTypeQuery

  -- Полный запрос
  let finalQuery = baseQuery ++ conditions ++ " ORDER BY ads.id ASC"

  [Only totalCount] <- query_ dataBase (fromString $ "SELECT COUNT(id) FROM (" ++ finalQuery ++ ") AS filteredAds") :: IO [Only Integer]
  -- Выводим количество найденных записей
  putStrLn $ "\nНайдено объявлений: " ++ show totalCount

  -- Если есть записи, выполняем основной запрос
  if totalCount > 0
    then do
      -- Основной запрос остается без изменений
      rawResults <- query_ dataBase (fromString finalQuery) :: IO [RawAdData]
      let adsWithAddress = map convertRawToAdWithAddress rawResults
      putStrLn "\n----- Отфильтрованные Объявления -----\n"
      mapM_ printAdWithAddress adsWithAddress

      putStrLn "\nВведите номер объявления для подробной информации или нажмите Enter для возврата:"
      choice <- getLine
      case reads choice :: [(Int, String)] of
        [(n, "")] -> do
          let selectedAd = find (\awa -> adId (ad awa) == fromIntegral n) adsWithAddress
          case selectedAd of
            Just adWithAddr -> do
              clearCLI
              getAdById dataBase (adId (ad adWithAddr))
              putStrLn "Хотите приобрести объект?"
              putStrLn "1) Да"
              putStrLn "2) Нет"

              action <- getLine
              case action of
                "1" -> do
                  putStrLn "функционал не реализован"
                _ -> do
                  putStrLn "функционал не реализован"

            Nothing -> do
              putStrLn "Объявление не найдено"
              filterAvailableAds
        _ -> return ()
    else
      putStrLn "Нет объявлений, соответствующих выбранным фильтрам."
  close dataBase
