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
  adCost        :: Float,
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
        putStrLn "Хотите отклонить сделку?"
        putStrLn "1) Да"
        putStrLn "_) Нет"

        choice <- getLine
        case choice of
          "1" -> do
            putStrLn "\nВыберите ID сделки для отклонения или введите 'q' для отмены:"
            input <- getLine
            case input of
              "q" -> return ()
              _ -> case reads input of
                [(n, "")] -> if n `elem` ids
                            then do
                              deal <- query dataBase "SELECT status FROM deals WHERE id = ?" (Only n) :: IO [Only String]
                              case deal of
                                [Only status] -> if status `elem` ["close", "reject"]
                                               then putStrLn "Нельзя отклонить сделку с таким статусом"
                                               else do
                                                 execute dataBase "UPDATE deals SET status = 'rejected' WHERE id = ?" (Only n)
                                                 putStrLn "Сделка успешно отклонена"
                                _ -> putStrLn "Сделка не найдена"
                            else putStrLn "Неверный ID сделки"
                _ -> putStrLn "Неверный ввод"
          _ -> return ()
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
        ++ "WHERE seller = ? or buyer = ?"
  deals <- query dataBase baseQuery (selfId, selfId) :: IO [Deal]
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
  putStrLn $ "|  ID сделки:\t" ++ show (dealId deal)
  putStrLn $ "|  Статус:\t\t" ++ show (dealStatus deal)
  putStrLn $ "|  Дата:\t\t" ++ dealDate deal
  putStrLn $ "|  Итоговая цена:\t" ++ show (dealFinalCost deal) ++ " RUB"
  putStrLn $ "|  ID объекта:\t" ++ show (adObjectId deal)
  putStrLn $ "|  Тип объекта:\t" ++ show (adObjectType deal)
  putStrLn $ "|  Цена объявления:\t" ++ show (adCost deal) ++ " RUB"
  putStrLn $ "|  Описание:\t" ++ adDescription deal
  putStrLn $ "|  Тип сделки:\t" ++ show (adDealType deal)
  putStrLn $ "|  Покупатель:\t" ++ buyerName deal ++ " " ++ buyerSurName deal
  putStrLn "+-------------------------------"
