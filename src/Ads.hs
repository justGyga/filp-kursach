{-# LANGUAGE OverloadedStrings #-}

module Ads
  ( Ad(..)
  , Address(..)
  , AdWithAddress(..)
  , getMyAds
  , viewAvailableAds
  , filterAvailableAds
  ) where

import           Commons                        (clearCLI)
import           Control.Monad                  (when)
import           Data.Maybe                     (isJust, fromJust)
import           Data.String                    (fromString)
import           Data.Text                      (Text, unpack)
import qualified Data.Text                      as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow ()
import           Database.SQLite.Simple.ToField (toField)
import           SQLplotter                     (getUserSession)

-- | Структура данных для адреса, соответствующая таблице `addresses`.
data Address = Address
  { addressId   :: Integer       -- ^ Уникальный идентификатор адреса
  , state       :: String        -- ^ Регион
  , city        :: String        -- ^ Город
  , district    :: String        -- ^ Район
  , postalCode  :: String        -- ^ Почтовый код
  , streetName  :: String        -- ^ Название улицы
  , houseNumber :: String        -- ^ Номер дома
  , entrance    :: Maybe Integer -- ^ Подъезд (может быть NULL)
  , doorNumber  :: Maybe Integer -- ^ Номер двери (может быть NULL)
  } deriving (Show)

-- | Экземпляр `FromRow` для `Address`, позволяет считывать записи из базы данных.
instance FromRow Address where
  fromRow = Address <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

-- | Экземпляр `ToRow` для `Address`, позволяет записывать записи в базу данных.
instance ToRow Address where
  toRow (Address addressId state city district postalCode streetName houseNumber entrance doorNumber) =
    toRow (addressId, state, city, district, postalCode, streetName, houseNumber, entrance, doorNumber)

-- | Структура данных для объявления, соответствующая таблице `ads`.
data Ad = Ad
  { adId        :: Integer  -- ^ Уникальный идентификатор объявления
  , seller      :: Integer  -- ^ Идентификатор продавца (пользователя)
  , objectId    :: Integer  -- ^ Идентификатор объекта недвижимости
  , objectType  :: Integer  -- ^ Тип объекта недвижимости (1: Квартира, 2: Дом, 3: Земельный участок, 4: Гараж, 5: Коммерческая недвижимость)
  , cost        :: Float    -- ^ Стоимость объекта
  , description :: String   -- ^ Описание объявления
  } deriving (Show)

-- | Экземпляр `FromRow` для `Ad`, позволяет считывать записи из базы данных.
instance FromRow Ad where
  fromRow = Ad <$> field <*> field <*> field <*> field <*> field <*> field

-- | Экземпляр `ToRow` для `Ad`, позволяет записывать записи в базу данных.
instance ToRow Ad where
  toRow (Ad adId seller objectId objectType cost description) =
    toRow (adId, seller, objectId, objectType, cost, description)

-- | Структура данных, объединяющая объявление и его адрес.
data AdWithAddress = AdWithAddress
  { ad         :: Ad        -- ^ Объявление
  , address    :: Address   -- ^ Адрес объекта недвижимости
  , objectType :: Integer   -- ^ Тип объекта недвижимости
  , area       :: Int       -- ^ Площадь объекта
  } deriving (Show)

-- | Экземпляр `FromRow` для `AdWithAddress`, позволяет считывать записи из базы данных.
instance FromRow AdWithAddress where
  fromRow = AdWithAddress <$> fromRow <*> fromRow <*> field <*> field

-- | Функция для получения и отображения объявлений пользователя.
getMyAds :: IO ()
getMyAds = do
  dataBase <- open "local.db"                      -- Открываем соединение с базой данных
  myId <- getUserSession                            -- Получаем ID текущего пользователя из сессии
  ads <- query dataBase "SELECT * FROM ads WHERE seller = ?;" (Only myId) :: IO [Ad] -- Извлекаем объявления пользователя
  if null ads
    then putStrLn "У вас нет активных объявлений."   -- Если объявлений нет, выводим сообщение
    else mapM_ printAd ads                           -- Иначе, выводим каждое объявление
  close dataBase                                    -- Закрываем соединение с базой данных

-- | Функция для просмотра всех доступных объявлений с их адресами.
viewAvailableAds :: IO ()
viewAvailableAds = do
  dataBase <- open "local.db"                      -- Открываем соединение с базой данных
  ads <- query_ dataBase "SELECT * FROM ads;" :: IO [Ad] -- Извлекаем все объявления

  if null ads
    then putStrLn "Нет доступных объявлений для просмотра." -- Если объявлений нет, выводим сообщение
    else do
      adsWithAddress <- mapM (getAdWithAddress dataBase) ads -- Для каждого объявления получаем его адрес и площадь
      clearCLI
      putStrLn "\n----- Доступные Объявления -----\n"
      mapM_ printAdWithAddress adsWithAddress             -- Выводим каждое объявление с адресом и площадью

  close dataBase                                        -- Закрываем соединение с базой данных

-- | Функция, объединяющая объявление и его адрес.
getAdWithAddress :: Connection -> Ad -> IO AdWithAddress
getAdWithAddress conn ad = do
  addr <- getAddress conn ad                             -- Получаем адрес для объявления
  -- Здесь мы не получаем площадь, поскольку viewAvailableAds не использует фильтрацию
  -- Площадь будет установлена в 0 или другим значением по умолчанию
  return $ AdWithAddress ad addr (objectType ad) 0      -- Возвращаем объединённую структуру с площадью = 0

-- | Функция для получения адреса объекта на основе типа объекта и его ID.
getAddress :: Connection -> Ad -> IO Address
getAddress conn ad = case objectType ad of
  1 -> getAddressFromFlats conn (objectId ad)             -- Если объект - квартира
  2 -> getAddressFromHouses conn (objectId ad)            -- Если объект - дом
  3 -> getAddressFromLandPlots conn (objectId ad)         -- Если объект - земельный участок
  4 -> getAddressFromGarages conn (objectId ad)           -- Если объект - гараж
  5 -> getAddressFromCommercialRealEstates conn (objectId ad) -- Если объект - коммерческая недвижимость
  _ -> return defaultAddress                              -- Неизвестный тип объекта

-- | Функция для получения адреса из таблицы `flats`.
getAddressFromFlats :: Connection -> Integer -> IO Address
getAddressFromFlats conn objId = do
  results <- query conn "SELECT addressId FROM flats WHERE id = ?;" (Only objId) :: IO [Only Integer]
  case results of
    [Only addrId] -> getAddressById conn addrId           -- Если адрес найден, получаем его детали
    _             -> return defaultAddress                -- Иначе, возвращаем дефолтный адрес

-- | Функция для получения адреса из таблицы `houses`.
getAddressFromHouses :: Connection -> Integer -> IO Address
getAddressFromHouses conn objId = do
  results <- query conn "SELECT addressId FROM houses WHERE id = ?;" (Only objId) :: IO [Only Integer]
  case results of
    [Only addrId] -> getAddressById conn addrId           -- Если адрес найден, получаем его детали
    _             -> return defaultAddress                -- Иначе, возвращаем дефолтный адрес

-- | Функция для получения адреса из таблицы `landPlot`.
getAddressFromLandPlots :: Connection -> Integer -> IO Address
getAddressFromLandPlots conn objId = do
  results <- query conn "SELECT addressId FROM landPlot WHERE id = ?;" (Only objId) :: IO [Only Integer]
  case results of
    [Only addrId] -> getAddressById conn addrId           -- Если адрес найден, получаем его детали
    _             -> return defaultAddress                -- Иначе, возвращаем дефолтный адрес

-- | Функция для получения адреса из таблицы `garages`.
getAddressFromGarages :: Connection -> Integer -> IO Address
getAddressFromGarages conn objId = do
  results <- query conn "SELECT addressId FROM garages WHERE id = ?;" (Only objId) :: IO [Only Integer]
  case results of
    [Only addrId] -> getAddressById conn addrId           -- Если адрес найден, получаем его детали
    _             -> return defaultAddress                -- Иначе, возвращаем дефолтный адрес

-- | Функция для получения адреса из таблицы `commercialRealEstates`.
getAddressFromCommercialRealEstates :: Connection -> Integer -> IO Address
getAddressFromCommercialRealEstates conn objId = do
  results <- query conn "SELECT addressId FROM commercialRealEstates WHERE id = ?;" (Only objId) :: IO [Only Integer]
  case results of
    [Only addrId] -> getAddressById conn addrId           -- Если адрес найден, получаем его
    _             -> return defaultAddress                -- Иначе, возвращаем дефолтный адрес

-- | Функция для получения полной информации об адресе по `addressId`.
getAddressById :: Connection -> Integer -> IO Address
getAddressById conn addrId = do
  results <- query conn "SELECT * FROM addresses WHERE id = ?;" (Only addrId) :: IO [Address]
  case results of
    [addr] -> return addr                                 -- Если адрес найден, возвращаем его
    _      -> return defaultAddress                      -- Иначе, возвращаем дефолтный адрес

-- | Дефолтный адрес для случаев, когда информация недоступна.
defaultAddress :: Address
defaultAddress = Address
  { addressId    = -1
  , state        = "Неизвестно"
  , city         = "Неизвестно"
  , district     = "Неизвестно"
  , postalCode   = "Неизвестно"
  , streetName   = "Неизвестно"
  , houseNumber  = "Неизвестно"
  , entrance     = Nothing
  , doorNumber   = Nothing
  }

-- | Вспомогательная функция для форматирования и вывода объявления без адреса.
printAd :: Ad -> IO ()
printAd ad = do
  putStrLn $ "ID Объявления:\t" ++ show (adId ad)
  putStrLn $ "ID Продавца:\t" ++ show (seller ad)
  putStrLn $ "Тип Объекта:\t" ++ showObjectType (objectType ad)
  putStrLn $ "ID Объекта:\t" ++ show (objectId ad)
  putStrLn $ "Стоимость:\t" ++ show (cost ad) ++ " RUB"
  putStrLn $ "Описание:\t" ++ description ad
  putStrLn "-----------------------------------"

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
    Nothing  -> putStrLn "Подъезд:\t\tНе указано"
  case doorNumber addr of
    Just dn -> putStrLn $ "Номер Двери:\t" ++ show dn
    Nothing -> putStrLn "Номер Двери:\t\tНе указан"
  putStrLn $ "Площадь:\t" ++ show area ++ " кв.м."
  putStrLn "-----------------------------------"

-- | Функция для преобразования `objectType` в читаемый формат.
showObjectType :: Integer -> String
showObjectType 1 = "Квартира"
showObjectType 2 = "Дом"
showObjectType 3 = "Земельный участок"
showObjectType 4 = "Гараж"
showObjectType 5 = "Коммерческая недвижимость"
showObjectType _ = "Неизвестный тип объекта"

-- | Новая функция для фильтрации доступных объявлений
filterAvailableAds :: IO ()
filterAvailableAds = do
  dataBase <- open "local.db"

  -- Получение доступных фильтров
  districts <- query_ dataBase "SELECT DISTINCT district FROM addresses;" :: IO [Only Text]
  objectTypes <- query_ dataBase "SELECT DISTINCT objectType FROM ads;" :: IO [Only Integer]

  -- Сбор фильтров от пользователя
  putStrLn "\n----- Фильтрация Объявлений -----\n"
  putStrLn "\n----- Район -----\n"

  -- Фильтр по району
  putStrLn "1. Выбрать район"
  putStrLn "2. Пропустить"
  districtChoice <- getLine
  districtFilter <- case districtChoice of
    "1" -> do
      putStrLn "Доступные районы:"
      mapM_ (\(i, d) -> putStrLn $ show i ++ ". " ++ T.unpack d) (zip [1..] (map fromOnly districts))
      putStrLn "Введите номер района:"
      idx <- getLine
      let selected = lookup (read idx :: Int) (zip [1..] (map fromOnly districts))
      case selected of
        Just d  -> return (Just d)
        Nothing -> putStrLn "Неверный выбор района." >> return Nothing
    _   -> return Nothing

  putStrLn "\n----- Тип объекта -----\n"
  putStrLn "1. Выбрать тип объекта"
  putStrLn "2. Пропустить"
  objectTypeChoice <- getLine
  objectTypeFilter <- case objectTypeChoice of
    "1" -> do
      putStrLn "Доступные типы объектов:"
      mapM_ (\(i, ot) -> putStrLn $ show i ++ ". " ++ showObjectType ot) (zip [1..] (map fromOnly objectTypes))
      putStrLn "Введите номер типа объекта:"
      idx <- getLine
      let selected = lookup (read idx :: Int) (zip [1..] (map fromOnly objectTypes))
      case selected of
        Just ot -> return (Just ot)
        Nothing -> putStrLn "Неверный выбор типа объекта." >> return Nothing
    _   -> return Nothing

  putStrLn "\n----- Максимальная стоимость -----\n"
  putStrLn "1. Ввести значение"
  putStrLn "2. Пропустить"
  maxCostChoice <- getLine
  maxCostFilter <- case maxCostChoice of
    "1" -> do
      putStrLn "Введите максимальную стоимость:"
      input <- getLine
      case reads input :: [(Float, String)] of
        [(mc, "")] -> return (Just mc)
        _          -> putStrLn "Некорректное значение." >> return Nothing
    _   -> return Nothing

  putStrLn "\n----- Минимальная площадь объекта -----\n"
  putStrLn "1. Ввести значение"
  putStrLn "2. Пропустить"
  minAreaChoice <- getLine
  minAreaFilter <- case minAreaChoice of
    "1" -> do
      putStrLn "Введите минимальную площадь объекта:"
      input <- getLine
      case reads input :: [(Int, String)] of
        [(ma, "")] -> return (Just ma)
        _          -> putStrLn "Некорректное значение." >> return Nothing
    _   -> return Nothing

  -- Определение базового запроса с UNION ALL
  let baseQuery = "SELECT ads.*, addresses.*, objects.area AS objectArea " ++
                  "FROM ads " ++
                  "JOIN ( " ++
                  "  SELECT id, objectType, area, addressId FROM flats " ++
                  "  UNION ALL " ++
                  "  SELECT id, objectType, area, addressId FROM houses " ++
                  "  UNION ALL " ++
                  "  SELECT id, objectType, area, addressId FROM landPlot " ++
                  "  UNION ALL " ++
                  "  SELECT id, objectType, area, addressId FROM garages " ++
                  "  UNION ALL " ++
                  "  SELECT id, objectType, area, addressId FROM commercialRealEstates " ++
                  ") AS objects ON ads.objectId = objects.id " ++
                  "JOIN addresses ON objects.addressId = addresses.id "

      -- Построение условий WHERE
      let conditions = concat [
            "WHERE 1=1 ",
            case objectTypeFilter of
              Just ot -> "AND objects.objectType = ? "
              Nothing -> "",
            if isJust districtFilter then "AND addresses.district = ? " else "",
            if isJust maxCostFilter then "AND ads.cost <= ? " else "",
            if isJust minAreaFilter then "AND objects.area >= ? " else ""
          ]

      -- Полный запрос
      let finalQuery = baseQuery ++ conditions ++ ";"

      -- Собираем параметры
          params = concat [
                    case objectTypeFilter of
                      Just ot -> [toField ot]
                      Nothing -> [],
                    maybe [] (\d -> [toField d]) districtFilter,
                    maybe [] (\mc -> [toField mc]) maxCostFilter,
                    maybe [] (\ma -> [toField ma]) minAreaFilter
                  ]

  -- Вывод запроса и параметров для отладки
  putStrLn "\nВыполняемый SQL-запрос:"
  putStrLn finalQuery
  putStrLn "\nПараметры запроса:"
  mapM_ print params

  -- Выполнение запроса
  adsWithAddress <- query dataBase (fromString finalQuery) params :: IO [AdWithAddress]

  -- clearCLI -- Временно отключено для отладки
  if null adsWithAddress
    then putStrLn "Нет объявлений, соответствующих выбранным фильтрам."
    else do
      putStrLn "\n----- Отфильтрованные Объявления -----\n"
      mapM_ printAdWithAddress adsWithAddress

  putStrLn "\n1. Назад"
  backChoice <- getLine
  when (backChoice == "1") filterAvailableAds

  close dataBase
