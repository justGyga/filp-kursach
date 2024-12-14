-- src/Ads.hs

module Ads
  ( Ad(..)
  , getMyAds
  , viewAvailableAds
  ) where

import           Data.String                    (fromString)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToRow
import           SQLplotter                     (getUserSession)

-- Определение типа данных Ad с автоматическим созданием экземпляра Show
data Ad = Ad
  { adId        :: Integer
  , seller      :: Integer
  , objectId    :: Integer
  , objectType  :: Integer
  , cost        :: Float
  , description :: String
  } deriving (Show)

-- Экземпляр FromRow для Ad
instance FromRow Ad where
  fromRow = Ad <$> field <*> field <*> field <*> field <*> field <*> field

-- Экземпляр ToRow для Ad
instance ToRow Ad where
  toRow (Ad adId seller objectId objectType cost description) =
    toRow (adId, seller, objectId, objectType, cost, description)

-- Функция для получения и вывода объявлений пользователя
getMyAds :: IO ()
getMyAds = do
  dataBase <- open "local.db"
  myId <- getUserSession
  ads <- query dataBase (fromString "SELECT * FROM ads WHERE seller = ?;") (Only myId) :: IO [Ad]
  if null ads
    then putStrLn "У вас нет активных объявлений."
    else mapM_ printAd ads
  close dataBase

-- Функция для просмотра всех доступных объявлений
viewAvailableAds :: IO ()
viewAvailableAds = do
  dataBase <- open "local.db"
  ads <- query_ dataBase (fromString "SELECT * FROM ads;") :: IO [Ad]
  if null ads
    then putStrLn "Нет доступных объявлений для просмотра."
    else do
      putStrLn "\n----- Доступные Объявления -----\n"
      mapM_ printAd ads
  close dataBase

-- Вспомогательная функция для форматирования и вывода объявления
printAd :: Ad -> IO ()
printAd ad = do
  putStrLn $ "ID Объявления: " ++ show (adId ad)
  putStrLn $ "ID Продавца: " ++ show (seller ad)
  putStrLn $ "Тип Объекта: " ++ showObjectType (objectType ad)
  putStrLn $ "ID Объекта: " ++ show (objectId ad)
  putStrLn $ "Стоимость: " ++ show (cost ad) ++ " RUB"
  putStrLn $ "Описание: " ++ description ad
  putStrLn "-----------------------------------"

-- Функция для преобразования objectType в читаемый формат
showObjectType :: Integer -> String
showObjectType 1 = "Квартира"
showObjectType 2 = "Дом"
showObjectType 3 = "Земельный участок"
showObjectType 4 = "Гараж"
showObjectType _ = "Неизвестный тип объекта"
