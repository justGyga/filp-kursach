{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module GetAdById where

import Data.String (fromString)
import Database.SQLite.Simple

data Ad = Ad
  { adId :: Integer,
    adObjectId :: Integer,
    adObjectType :: Integer,
    adCost :: Float,
    adDescription :: String,
    adSellerName :: String,
    adSellerSurname :: String
  }
  deriving (Show)

instance FromRow Ad where
  fromRow = Ad <$> field <*> field <*> field <*> field <*> field <*> field <*> field

data Flat = Flat
  { flatId :: Integer,
    flatArea :: Integer,
    flatRooms :: Integer,
    flatFloor :: Integer,
    flatFloorsCount :: Integer,
    flatBalconyArea :: Integer,
    flatAddressId :: Integer,
    flatOt :: Integer
  }
  deriving (Show)

instance FromRow Flat where
  fromRow = Flat <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data House = House
  { houseId :: Integer,
    houseAreaType :: Integer,
    houseArea :: Integer,
    houseRooms :: Integer,
    houseFloor :: Integer,
    houseAddressId :: Integer,
    houseBasementArea :: Integer,
    houseOt :: Integer
  }
  deriving (Show)

instance FromRow House where
  fromRow = House <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data LandPlot = LandPlot
  { landPlotId :: Integer,
    landPlotArea :: Integer,
    landPlotCategory :: Integer,
    landPlotAddressId :: Integer,
    landPlotOt :: Integer
  }
  deriving (Show)

instance FromRow LandPlot where
  fromRow = LandPlot <$> field <*> field <*> field <*> field <*> field

data Garage = Garage
  { garageId :: Integer,
    garageArea :: Integer,
    garageSecurity :: Bool,
    garageAddressId :: Integer,
    garageOt :: Integer
  }
  deriving (Show)

instance FromRow Garage where
  fromRow = Garage <$> field <*> field <*> field <*> field <*> field

data CommercialRealEstate = CommercialRealEstate
  { commercialRealEstateId :: Integer,
    commercialRealEstateArea :: Int,
    commercialRealEstateObjectType :: Integer,
    commercialRealEstateAddressId :: Integer,
    commercialRealEstateOt :: Integer
  }
  deriving (Show)

instance FromRow CommercialRealEstate where
  fromRow = CommercialRealEstate <$> field <*> field <*> field <*> field <*> field

data Address = Address
  { addressId :: Integer,
    addressState :: String,
    addressCity :: String,
    addressDistrict :: String,
    addressPostalCode :: String,
    addressStreetName :: String,
    addressHouseNumber :: String,
    addressEntrance :: Maybe Integer,
    addressDoorNumber :: Maybe Integer
  }
  deriving (Show)

instance FromRow Address where
  fromRow = Address <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

defaultAddress :: Address
defaultAddress =
  Address
    { addressId = -1,
      addressState = "Неизвестно",
      addressCity = "Неизвестно",
      addressDistrict = "Неизвестно",
      addressPostalCode = "Неизвестно",
      addressStreetName = "Неизвестно",
      addressHouseNumber = "Неизвестно",
      addressEntrance = Nothing,
      addressDoorNumber = Nothing
    }

getAdById :: Connection -> Integer -> IO ()
getAdById dataBase adId = do
  let adQuery =
        fromString $
          "SELECT ads.id, ads.\"objectId\", ads.\"objectType\", ads.cost, ads.description, us.name, us.surname FROM ads INNER JOIN users us ON ads.seller = us.id WHERE ads.id=? LIMIT 1;"

  result <- query dataBase adQuery (Only adId) :: IO [Ad]
  case result of
    [ad] -> do
      printAd ad
      case adObjectType ad of
        1 -> do
          flat <- getFlat dataBase (adObjectId ad)
          printFlat flat
          address <- getAddressById dataBase (flatAddressId flat)
          printAddress address
        2 -> do
          house <- getHouse dataBase (adObjectId ad)
          printHouse house
          address <- getAddressById dataBase (houseAddressId house)
          printAddress address
        3 -> do
          landPlot <- getLandPlot dataBase (adObjectId ad)
          printLandPlot landPlot
          address <- getAddressById dataBase (landPlotAddressId landPlot)
          printAddress address
        4 -> do
          garage <- getGarage dataBase (adObjectId ad)
          printGarage garage
          address <- getAddressById dataBase (garageAddressId garage)
          printAddress address
        5 -> do
          commercialRealEstate <- getCommercialRealEstate dataBase (adObjectId ad)
          printCommercialRealEstate commercialRealEstate
          address <- getAddressById dataBase (commercialRealEstateAddressId commercialRealEstate)
          printAddress address
        _ -> putStrLn "Неизвестный тип объекта"
    _ -> putStrLn "Объявление не найдено"

printAd :: Ad -> IO ()
printAd ad = do
  putStrLn $ "+- Объявление ---------"
  putStrLn $ "|  ID Объявления:\t" ++ show (adId ad)
  putStrLn $ "|  Стоимость:\t" ++ show (adCost ad) ++ " RUB"
  putStrLn $ "|  Описание:\t" ++ adDescription ad
  putStrLn $ "|  Продавец:\t" ++ adSellerName ad ++ " " ++ adSellerSurname ad

getFlat :: Connection -> Integer -> IO Flat
getFlat dataBase objectId = do
  result <- query dataBase (fromString $ "SELECT * FROM flats WHERE id=? LIMIT 1;") (Only objectId) :: IO [Flat]
  case result of
    [flat] -> return flat
    _ ->
      return $
        Flat
          { flatId = -1,
            flatArea = 0,
            flatRooms = 0,
            flatFloor = 0,
            flatAddressId = -1,
            flatOt = 1
          }

printFlat :: Flat -> IO ()
printFlat flat = do
  putStrLn $ "+- Квартира ---------"
  putStrLn $ "|  Площадь:\t" ++ show (flatArea flat) ++ " кв.м"
  putStrLn $ "|  Количество комнат:\t" ++ show (flatRooms flat)
  putStrLn $ "|  Этаж:\t" ++ show (flatFloor flat) ++ " из " ++ show (flatFloorsCount flat)
  putStrLn $ "|  Площадь балкона:\t" ++ show (flatBalconyArea flat) ++ " кв.м"

getHouse :: Connection -> Integer -> IO House
getHouse dataBase objectId = do
  result <- query dataBase (fromString $ "SELECT * FROM houses WHERE id=? LIMIT 1;") (Only objectId) :: IO [House]
  case result of
    [house] -> return house
    _ ->
      return $
        House
          { houseId = -1,
            houseArea = 0,
            houseRooms = 0,
            houseFloor = 0,
            houseAddressId = -1,
            houseOt = 2
          }

printHouse :: House -> IO ()
printHouse house = do
  putStrLn $ "+- Дом ---------"
  putStrLn $ "|  Площадь:\t" ++ show (houseArea house) ++ " кв.м"
  putStrLn $ "|  Количество комнат:\t" ++ show (houseRooms house)
  putStrLn $ "|  Число этажей:\t" ++ show (houseFloor house)
  putStrLn $ "|  Площадь подвала:\t" ++ show (houseBasementArea house) ++ " кв.м"
  putStrLn $ "|  Тип площади:\t" ++ show (houseAreaType house)

getLandPlot :: Connection -> Integer -> IO LandPlot
getLandPlot dataBase objectId = do
  result <- query dataBase (fromString $ "SELECT * FROM \"landPlot\" WHERE id=? LIMIT 1;") (Only objectId) :: IO [LandPlot]
  case result of
    [landPlot] -> return landPlot
    _ ->
      return $
        LandPlot
          { landPlotId = -1,
            landPlotArea = 0,
            landPlotCategory = 0,
            landPlotAddressId = -1,
            landPlotOt = 3
          }

printLandPlot :: LandPlot -> IO ()
printLandPlot landPlot = do
  putStrLn $ "+- Участок ---------"
  putStrLn $ "|  Площадь:\t" ++ show (landPlotArea landPlot) ++ " кв.м"
  putStrLn $ "|  Категория:\t" ++ show (landPlotCategory landPlot)

getGarage :: Connection -> Integer -> IO Garage
getGarage dataBase objectId = do
  result <- query dataBase (fromString $ "SELECT * FROM garages WHERE id=? LIMIT 1;") (Only objectId) :: IO [Garage]
  case result of
    [garage] -> return garage
    _ ->
      return $
        Garage
          { garageId = -1,
            garageArea = 0,
            garageSecurity = False,
            garageAddressId = -1,
            garageOt = 4
          }

printGarage :: Garage -> IO ()
printGarage garage = do
  putStrLn $ "+- Гараж ---------"
  putStrLn $ "|  Площадь:\t" ++ show (garageArea garage) ++ " кв.м"
  putStrLn $ "|  Безопасность:\t" ++ show (garageSecurity garage)

getCommercialRealEstate :: Connection -> Integer -> IO CommercialRealEstate
getCommercialRealEstate dataBase objectId = do
  result <- query dataBase (fromString $ "SELECT * FROM \"commercialRealEstates\" WHERE id=? LIMIT 1;") (Only objectId) :: IO [CommercialRealEstate]
  case result of
    [commercialRealEstate] -> return commercialRealEstate
    _ ->
      return $
        CommercialRealEstate
          { commercialRealEstateId = -1,
            commercialRealEstateArea = 0,
            commercialRealEstateObjectType = 0,
            commercialRealEstateAddressId = -1,
            commercialRealEstateOt = 5
          }

printCommercialRealEstate :: CommercialRealEstate -> IO ()
printCommercialRealEstate commercialRealEstate = do
  putStrLn $ "+- Торговое помещение ---------"
  putStrLn $ "|  Площадь:\t" ++ show (commercialRealEstateArea commercialRealEstate) ++ " кв.м"
  putStrLn $ "|  Тип объекта:\t" ++ show (commercialRealEstateObjectType commercialRealEstate)

getAddressById :: Connection -> Integer -> IO Address
getAddressById dataBase addressId = do
  result <- query dataBase (fromString $ "SELECT * FROM addresses WHERE id=? LIMIT 1;") (Only addressId) :: IO [Address]
  case result of
    [address] -> return address
    _ -> return defaultAddress

printAddress :: Address -> IO ()
printAddress address = do
  putStrLn $ "+- Адрес:---------"
  putStrLn $ "|  Область:\t" ++ addressState address
  putStrLn $ "|  Город:\t" ++ addressCity address
  putStrLn $ "|  Район:\t" ++ addressDistrict address
  putStrLn $ "|  Почтовый индекс:\t" ++ addressPostalCode address
  putStrLn $ "|  Улица:\t" ++ addressStreetName address
  putStrLn $ "|  Дом:\t" ++ addressHouseNumber address
  case addressEntrance address of
    Just ent -> putStrLn $ "|  Подъезд:\t" ++ show ent
    Nothing -> putStrLn "|  Подъезд:\t\tНе указано"
  case addressDoorNumber address of
    Just dn -> putStrLn $ "|  Номер Двери:\t" ++ show dn
    Nothing -> putStrLn "|  Номер Двери:\t\tНе указан"
  putStrLn $ "+-----------------"
