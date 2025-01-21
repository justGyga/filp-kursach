{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module GetProfile where

import           Data.String            (fromString)
import           Database.SQLite.Simple
import           SQLplotter             (getUserSession)

data SimpleUser = SimpleUser {
  userId      :: Integer,
  userName    :: String,
  userSurname :: String,
  userEmail   :: String,
  userBalance :: Float
} deriving (Show)

instance FromRow SimpleUser where
  fromRow = SimpleUser <$> field <*> field <*> field <*> field <*> field

getProfile :: IO ()
getProfile = do
  dataBase <- open "local.db"
  id <- getUserSession
  if id == -1
    then putStrLn "Пользователь не авторизован"
    else do
      getProfileService dataBase id
  close dataBase

getProfileService :: Connection -> Integer -> IO ()
getProfileService dataBase id = do
  let userQuery = fromString "SELECT users.id, users.name, users.surname, users.email, wallets.balance FROM users INNER JOIN wallets ON users.wallet = wallets.id WHERE users.id = ? LIMIT 1;"
  users <- query dataBase userQuery (Only id) :: IO [SimpleUser]
  case users of
    [SimpleUser userId userName userSurname userEmail balance] -> do
      putStrLn "--- Профиль пользователя ---"
      putStrLn $ "ID: " ++ show userId
      putStrLn $ "Имя: " ++ userName
      putStrLn $ "Фамилия: " ++ userSurname
      putStrLn $ "Email: " ++ userEmail
      putStrLn $ "Баланс: " ++ show balance
    _ -> putStrLn "Пользователь не найден"
