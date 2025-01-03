{-# LANGUAGE OverloadedStrings #-}

module GetAdsList
  ( Ad (..),
    Address (..),
    AdWithAddress (..),
    viewAvailableAds,
    filterAvailableAds,
  )
where

import Commons (clearCLI)
import Data.List (find)
import Data.String (fromString)
import Data.Text (Text)
import DataTypes
  ( RawAdData (..),
    rawAdId,
    rawAddressId,
    rawCity,
    rawCost,
    rawDescription,
    rawDistrict,
    rawDoorNumber,
    rawEntrance,
    rawHouseNumber,
    rawObjectArea,
    rawObjectId,
    rawObjectType,
    rawPostalCode,
    rawSeller,
    rawState,
    rawStreetName,
  )
import Database.SQLite.Simple
import Enums (allAdObjectTypes)
import Filters (districtFilter, maxAreaFilter, maxCostFilter, minAreaFilter, minimalCostFilter, objectTypeFilter)
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
    description :: String
  }
  deriving (Show)

instance FromRow Ad where
  fromRow = Ad <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Ad where
  toRow (Ad adId seller objectId objectType cost description) =
    toRow (adId, seller, objectId, objectType, cost, description)

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

    let ad = Ad adId seller objectId objectType cost description
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
            description = rawDescription raw
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

-- | Функция для просмотра всех доступных объявлений с их адресами.
viewAvailableAds :: IO ()
viewAvailableAds = do
  dataBase <- open "local.db"
  ads <- query_ dataBase "SELECT * FROM ads;" :: IO [Ad] -- Извлекаем все объявления
  if null ads
    then putStrLn "Нет доступных объявлений для просмотра." -- Если объявлений нет, выводим сообщение
    else do
      adsWithAddress <- mapM (getAdWithAddress dataBase) ads -- Для каждого объявления получаем его адрес и площадь
      clearCLI
      putStrLn "\n----- Доступные Объявления -----\n"
      mapM_ printAdWithAddress adsWithAddress -- Выводим каждое объявление с адресом и площадью
  close dataBase -- Закрываем соединение с базой данных

-- | Функция, объединяющая объявление и его адрес.
getAdWithAddress :: Connection -> Ad -> IO AdWithAddress
getAdWithAddress conn ad = do
  addr <- getAddress conn ad -- Получаем адрес для объявления
  -- Здесь мы не получаем площадь, поскольку viewAvailableAds не использует фильтрацию
  -- Площадь будет установлена в 0 или другим значением по умолчанию
  return $ AdWithAddress ad addr (objectType ad) 0 -- Возвращаем объединённую структуру с площадью = 0

-- | Функция для получения адреса объекта на основе типа объекта и его ID.
getAddress :: Connection -> Ad -> IO Address
getAddress conn ad = case objectType ad of
  1 -> getAddressFromFlats conn (objectId ad) -- Если объект - квартира
  2 -> getAddressFromHouses conn (objectId ad) -- Если объект - дом
  3 -> getAddressFromLandPlots conn (objectId ad) -- Если объект - земельный участок
  4 -> getAddressFromGarages conn (objectId ad) -- Если объект - гараж
  5 -> getAddressFromCommercialRealEstates conn (objectId ad) -- Если объект - коммерческая недвижимость
  _ -> return defaultAddress -- Неизвестный тип объекта

-- | Функция дл получения адреса из таблицы `flats`.
getAddressFromFlats :: Connection -> Integer -> IO Address
getAddressFromFlats conn objId = do
  results <- query conn "SELECT addressId FROM flats WHERE id = ?;" (Only objId) :: IO [Only Integer]
  case results of
    [Only addrId] -> getAddressById conn addrId -- Если адрес найден, получаем его детали
    _ -> return defaultAddress -- Иначе, возвращаем дефолтный адрес

-- | Функция для получения адреса из таблицы `houses`.
getAddressFromHouses :: Connection -> Integer -> IO Address
getAddressFromHouses conn objId = do
  results <- query conn "SELECT addressId FROM houses WHERE id = ?;" (Only objId) :: IO [Only Integer]
  case results of
    [Only addrId] -> getAddressById conn addrId -- Если адрес найден, получаем его детали
    _ -> return defaultAddress -- Иначе, возвращаем дефолтный адрес

-- | Функция для получения адреса из таблицы `landPlot`.
getAddressFromLandPlots :: Connection -> Integer -> IO Address
getAddressFromLandPlots conn objId = do
  results <- query conn "SELECT addressId FROM landPlot WHERE id = ?;" (Only objId) :: IO [Only Integer]
  case results of
    [Only addrId] -> getAddressById conn addrId -- Если адрес найден, получаем его детали
    _ -> return defaultAddress -- Иначе, возвращаем дефолтный адрес

-- | Функция для получения адреса из таблицы `garages`.
getAddressFromGarages :: Connection -> Integer -> IO Address
getAddressFromGarages conn objId = do
  results <- query conn "SELECT addressId FROM garages WHERE id = ?;" (Only objId) :: IO [Only Integer]
  case results of
    [Only addrId] -> getAddressById conn addrId -- Если адрес найден, получаем его детали
    _ -> return defaultAddress -- Иначе, возвращаем дефолтный адрес

-- | Функция для получения адреса из таблицы `commercialRealEstates`.
getAddressFromCommercialRealEstates :: Connection -> Integer -> IO Address
getAddressFromCommercialRealEstates conn objId = do
  results <- query conn "SELECT addressId FROM commercialRealEstates WHERE id = ?;" (Only objId) :: IO [Only Integer]
  case results of
    [Only addrId] -> getAddressById conn addrId -- Если адрес найден, получаем его
    _ -> return defaultAddress -- Иначе, возвращаем дефолтный адрес

-- | Функция для получения полной информации об адресе по `addressId`.
getAddressById :: Connection -> Integer -> IO Address
getAddressById conn addrId = do
  results <- query conn "SELECT * FROM addresses WHERE id = ?;" (Only addrId) :: IO [Address]
  case results of
    [addr] -> return addr -- Если адрес найден, возвращаем его
    _ -> return defaultAddress -- Иначе, возвращаем дефолтный адрес

-- | Дефолтный адрес для случаев, когда информация недоступна.
defaultAddress :: Address
defaultAddress =
  Address
    { addressId = -1,
      state = "Неизвестно",
      city = "Неизвестно",
      district = "Неизвестно",
      postalCode = "Неизвестно",
      streetName = "Неизвестно",
      houseNumber = "Неизвестно",
      entrance = Nothing,
      doorNumber = Nothing
    }

-- | Вспомогательная функция для форматирования и вывода объявления вместе с адресом и площадью.
printAdWithAddress :: AdWithAddress -> IO ()
printAdWithAddress (AdWithAddress ad addr ot area) = do
  putStrLn $ "+- ID Объявления:\t" ++ show (adId ad)
  putStrLn $ "|  ID Продавца:\t" ++ show (seller ad)
  putStrLn $ "|  Тип Объекта:\t" ++ showObjectType ot
  putStrLn $ "|  ID Объекта:\t" ++ show (objectId ad)
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

-- | Функция для преобразования `objectType` в читаемый формат.
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
  clearCLI

  currentUserId <- getUserSession
  let sellerFilter = "ads.seller != " ++ show currentUserId

  let baseQuery =
        "SELECT "
          ++ "ads.id, "
          ++ "ads.\"objectId\", "
          ++ "ads.\"objectType\", "
          ++ "ads.seller, "
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
          ++ "WHERE "
          ++ sellerFilter -- Начало условий WHERE

  -- Построение условий WHERE и списка параметров
  let conditions = districtFilterQuery ++ objectTypeFilterQuery ++ minimalCostFilterQuery ++ maxCostFilterQuery ++ minAreaFilterQuery ++ maxAreaFilterQuery

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
            Nothing -> do
              putStrLn "Объявление не найдено"
              filterAvailableAds
        _ -> return ()
    else
      putStrLn "Нет объявлений, соответствующих выбранным фильтрам."
  close dataBase
