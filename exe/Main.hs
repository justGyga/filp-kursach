module Main where

import Commons (clearCLI)
import Data.String (fromString)
import Data.Text
import Database.SQLite.Simple
import Menu (startMenu)
import SQLplotter (addSessionTable, initializeDB, seedDB)

main :: IO ()
main = do
  initializeDB
  seedDB
  addSessionTable
  clearCLI
  listAllAdsWithDistricts
  startMenu

listAllAdsWithDistricts :: IO ()
listAllAdsWithDistricts = do
  dataBase <- open "local.db"
  ads <-
    query_
      dataBase
      ( fromString
          "SELECT ads.id, ads.description, addresses.district \
          \FROM ads \
          \JOIN flats ON ads.objectId = flats.id \
          \JOIN addresses ON flats.addressId = addresses.id;"
      ) ::
      IO [(Int, String, Text)]
  close dataBase
  if Prelude.null ads
    then putStrLn "Нет объявлений в базе данных."
    else do
      putStrLn "Список всех объявлений с районами:"
      mapM_ (\(id, desc, district) -> putStrLn $ show id ++ ": " ++ desc ++ " (" ++ unpack district ++ ")") ads
