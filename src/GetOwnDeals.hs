{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module GetOwnDeals where

import           Data.String            (fromString)
import           Database.SQLite.Simple
import           SQLplotter             (getUserSession)

data Deal = Deal {
  dealId        :: Integer,
  dealStatus    :: String,
  dealDate      :: String,
  dealFinalCost :: Float,
  adObjectId    :: Integer,
  adObjectType  :: Integer,
  adCost        :: Integer,
  adDescription :: String,
  adDealType    :: Integer,
  buyerName     :: String,
  buyerSurName  :: String
}

instance FromRow Deal where
  fromRow =
    Deal
      <$> field -- dealId
      <*> field -- dealStatus
      <*> field -- dealDate
      <*> field -- dealFinalCost
      <*> field -- adObjectId
      <*> field -- adObjectType
      <*> field -- adCost
      <*> field -- adDescription
      <*> field -- adDealType
      <*> field -- buyerName
      <*> field -- buyerSurName

getOwnDeals :: IO ()
getOwnDeals = do
    dataBase <- open "local.db"
    selfId <- getUserSession
    if selfId == -1
      then putStrLn "Пользователь не авторизован"
    else do
        ids <- getOwnDealsService dataBase selfId
        print ids
    close dataBase


getOwnDealsService :: Connection -> Integer -> IO [Integer]
getOwnDealsService dataBase selfId = do
  let baseQuery = fromString $ "SELECT "
        ++ "deals.id, status, date, finalCost,"
        ++ "\"objectId\", \"objectType\", cost,"
        ++ "description, \"dealType\", "
        ++ "buyers.name AS \"buyerName\","
        ++ "buyers.surname AS \"buyerSurname\" "
        ++ "FROM DEALS AS deals "
        ++ "LEFT JOIN ads ON \"adId\" = ads.id "
        ++ "LEFT JOIN users buyers ON buyer = buyers.id "
        ++ "INNER JOIN users sellers ON seller = sellers.id "
        ++ "WHERE seller = ?"
  deals <- query dataBase baseQuery (Only selfId) :: IO [Deal]
  if null deals
    then do
      putStrLn "У вас нет сделок."
      return []
    else do
      putStrLn "---- Ваши Сделки ----"
      mapM_ printDeals deals
      return $ map dealId deals

printDeals :: Deal -> IO ()
printDeals deal = do
  putStrLn $ "ID сделки:\t" ++ show (dealId deal)
  putStrLn $ "Статус:\t\t" ++ show (dealStatus deal)
  putStrLn $ "Дата:\t\t" ++ dealDate deal
  putStrLn $ "Итоговая цена:\t" ++ show (dealFinalCost deal) ++ " RUB"
  putStrLn $ "ID объекта:\t" ++ show (adObjectId deal)
  putStrLn $ "Тип объекта:\t" ++ show (adObjectType deal)
  putStrLn $ "Цена объявления:\t" ++ show (adCost deal) ++ " RUB"
  putStrLn $ "Описание:\t" ++ adDescription deal
  putStrLn $ "Тип сделки:\t" ++ show (adDealType deal)
  putStrLn $ "Покупатель:\t" ++ buyerName deal ++ " " ++ buyerSurName deal
  putStrLn "-----------------------------------"
