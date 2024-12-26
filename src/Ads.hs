{-# LANGUAGE OverloadedStrings #-}

module Ads
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
import Data.Text qualified as T
import Database.SQLite.Simple
import SQLplotter (getUserSession)
import Enums (getAdObjectType)

data RawAdData = RawAdData
  { rawAdId :: Integer,
    rawObjectId :: Integer,
    rawObjectType :: Integer,
    rawSeller :: Integer,
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
      <*> field -- seller
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
  putStrLn $ "ID Объявления:\t" ++ show (adId ad)
  putStrLn $ "ID Продавца:\t" ++ show (seller ad)
  putStrLn $ "Тип Объекта:\t" ++ showObjectType ot
  putStrLn $ "ID Объекта:\t" ++ show (objectId ad)
  putStrLn $ "Стоимость:\t" ++ show (cost ad) ++ " RUB"
  putStrLn $ "Описание:\t" ++ description ad
  putStrLn "----- Адрес Объекта -----"
  putStrLn $ "Регион:\t\t" ++ state addr
  putStrLn $ "Город:\t\t" ++ city addr
  putStrLn $ "Район:\t\t" ++ district addr
  putStrLn $ "Почтовый Код:\t" ++ postalCode addr
  putStrLn $ "Улица:\t\t" ++ streetName addr
  putStrLn $ "Номер Дома:\t" ++ houseNumber addr
  case entrance addr of
    Just ent -> putStrLn $ "Подъезд:\t" ++ show ent
    Nothing -> putStrLn "Подъезд:\t\tНе указано"
  case doorNumber addr of
    Just dn -> putStrLn $ "Номер Двери:\t" ++ show dn
    Nothing -> putStrLn "Номер Двери:\t\tНе указан"
  putStrLn $ "Площадь:\t" ++ show area ++ " кв.м."
  putStrLn "-----------------------------------"

-- | Функция для преобразования `objectType` в читаемый формат.
showObjectType :: Integer -> String
showObjectType n = case lookup n allObjectTypes of
  Just name -> name
  Nothing -> "Неизвестный тип объекта"

-- | Новая функция для фильтрации доступных объявлений
filterAvailableAds :: IO ()
filterAvailableAds = do
  dataBase <- open "local.db"

  -- Получение доступных фильтров
  districts <- query_ dataBase "SELECT DISTINCT district FROM addresses;" :: IO [Only Text]

  -- Сбор фильтров от пользователя
  putStrLn "\n----- Фильтрация Объявлений -----\n"
  putStrLn "\n----- Район -----\n"

  -- Фильтр по району
  putStrLn "1. Выбрать район"
  putStrLn "_. Пропустить"
  districtChoice <- getLine
  districtFilter <- case districtChoice of
    "1" -> do
      putStrLn "Доступные районы:"
      mapM_ (\(i, d) -> putStrLn $ show i ++ ". " ++ T.unpack d) (zip [1 ..] (map fromOnly districts))
      putStrLn "Введите номер района:"
      idx <- getLine
      case reads idx :: [(Int, String)] of
        [(n, "")] -> do
          -- Успешное преобразование в число
          let selected = lookup n (zip [1 ..] (map fromOnly districts))
          case selected of
            Just d -> return $ " AND addresses.district='" ++ T.unpack d ++ "'"
            Nothing -> putStrLn "Неверный номер района." >> return ""
        _ -> do
          -- Ошибка преобразования
          putStrLn "Некорректный ввод. Пожалуйста, введите число."
          return ""
    _ -> return ""

  putStrLn "\n----- Тип объекта -----\n"
  putStrLn "1. Выбрать тип объекта"
  putStrLn "_. Пропустить"
  objectTypeChoice <- getLine
  objectTypeFilter <- case objectTypeChoice of
    "1" -> do
      idx <- getAdObjectType
      return $ " AND ads.\"objectType\"=" ++ show idx
    _ -> return ""

  -- Фильтр по минимальной стоимости
  putStrLn "\n----- Минимальная стоимость -----\n"
  putStrLn "1. Ввести значение"
  putStrLn "_. Пропустить"
  minCostChoice <- getLine
  minCostFilter <- case minCostChoice of
    "1" -> do
      putStrLn "Введите минимальную стоимость:"
      input <- getLine
      case reads input :: [(Float, String)] of
        [(cost, "")] | cost >= 0 -> return $ " AND ads.cost >= " ++ show cost
        _ -> do
          putStrLn "Некорректное значение. Пожалуйста, введите положительное число."
          return ""
    _ -> return ""

  -- Фильтр по максимальной стоимости
  putStrLn "\n----- Максимальная стоимость -----\n"
  putStrLn "1. Ввести значение"
  putStrLn "_. Пропустить"
  maxCostChoice <- getLine
  maxCostFilter <- case maxCostChoice of
    "1" -> do
      putStrLn "Введите максимальную стоимость:"
      input <- getLine
      case reads input :: [(Float, String)] of
        [(cost, "")] | cost >= 0 -> return $ " AND ads.cost <= " ++ show cost
        _ -> do
          putStrLn "Некорректное значение. Пожалуйста, введите положительное число."
          return ""
    _ -> return ""

  -- Фильтр по минимальной площади
  putStrLn "\n----- Минимальная площадь объекта -----\n"
  putStrLn "1. Ввести значение"
  putStrLn "_. Пропустить"
  minAreaChoice <- getLine
  minAreaFilter <- case minAreaChoice of
    "1" -> do
      putStrLn "Введите минимальную площадь объекта:"
      input <- getLine
      case reads input :: [(Int, String)] of
        [(area, "")] | area >= 0 -> return $ " AND objs.area >= " ++ show area
        _ -> do
          putStrLn "Некорректное значение. Пожалуйста, введите положительное целое число."
          return ""
    _ -> return ""

  -- Фильтр по максимальной площади
  putStrLn "\n----- Максимальная площадь объекта -----\n"
  putStrLn "1. Ввести значение"
  putStrLn "_. Пропустить"
  maxAreaChoice <- getLine
  maxAreaFilter <- case maxAreaChoice of
    "1" -> do
      putStrLn "Введите максимальную площадь объекта:"
      input <- getLine
      case reads input :: [(Int, String)] of
        [(area, "")] | area >= 0 -> return $ " AND objs.area <= " ++ show area
        _ -> do
          putStrLn "Некорректное значение. Пожалуйста, введите положительное целое число."
          return ""
    _ -> return ""

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
  let conditions = districtFilter ++ objectTypeFilter ++ minCostFilter ++ maxCostFilter ++ minAreaFilter ++ maxAreaFilter

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
            Just ad -> do
              clearCLI
              printAdWithAddress ad
              filterAvailableAds
            Nothing -> do
              putStrLn "Объявление не найдено"
              filterAvailableAds
        _ -> return ()
    else
      putStrLn "Нет объявлений, соответствующих выбранным фильтрам."
  close dataBase

-- Добавим новую структуру для типов объектов
data ObjectType = Flat | House | LandPlot | Garage | Commercial
  deriving (Show, Eq)

-- Добавим список всех типов объектов
allObjectTypes :: [(Integer, String)]
allObjectTypes =
  [ (1, "Квартира"),
    (2, "Дом"),
    (3, "Земельный участок"),
    (4, "Гараж"),
    (5, "Коммерческая недвижимость")
  ]
