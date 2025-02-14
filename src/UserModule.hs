{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module UserModule where

import           Commons                (getEmail, getString)
import           Database.SQLite.Simple
import           Prelude                hiding (id)
import           SQLplotter             (addUserSession)
import           WalletModule

data User = User {id :: Int, name :: String, surname :: String, email :: String, password :: String, wallet :: Int} deriving (Show)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow User where
  toRow (User userId name surname email password wallet) =
    toRow (userId, name, surname, email, password, wallet)

signIn :: IO Bool
signIn = do
  dataBase <- open "local.db"
  result <- findAccount dataBase
  close dataBase
  return result

signUp :: IO Bool
signUp = do
  dataBase <- open "local.db"
  result <- createAccount dataBase
  close dataBase
  return result

findAccount :: Connection -> IO Bool
findAccount db = do
  email <- getEmail "Введите вашу электронную почту:"
  password <- getString "Введите ваш пароль:" False
  users <- query db "SELECT id FROM users WHERE email = ? AND password = ? LIMIT 1;" (email, password) :: IO [Only Integer]
  if null users
    then do
      putStrLn "Аккаунт не найден"
      putStrLn "1. Попробовать еще раз"
      putStrLn "_. Выйти"
      choice <- getLine
      case choice of
        "1" -> findAccount db
        _ -> do
          putStrLn "Выход..."
          return False
    else do
      putStrLn "Аккаунт найден"
      let Only userId = head users
      addUserSession userId
      return True

createAccount :: Connection -> IO Bool
createAccount db = do
  email <- getEmail "Введите вашу электронную почту:"
  password <- getString "Введите ваш пароль:" False
  verifyPassword <- getString "Повторите ваш пароль:" False

  let isPasswordVerified = password == verifyPassword
  if not isPasswordVerified
    then do
      putStrLn "Пароли не совпадают."
      putStrLn "1. Попробовать еще раз"
      putStrLn "_. Выйти"
      choice <- getLine
      case choice of
        "1" -> createAccount db
        _ -> do
          putStrLn "Выход..."
          return False
    else do
      name <- getString "Введите ваше имя:" False
      surname <- getString "Введите вашу фамилию:" False

      ids <- query_ db "SELECT id FROM users ORDER BY id DESC LIMIT 1;" :: IO [Only Integer]
      let newId =
            if null ids
              then 1
              else let Only lastId = head ids in lastId + 1

      -- putStrLn ("Ваш id: "++show newId)
      walletId <- createWallet db
      -- putStrLn ("Ваш ID кошелька: " ++ show walletId)
      execute db "INSERT INTO users (id, name, surname, email, password, wallet) VALUES (?, ?, ?, ?, ?, ?);" (newId, name, surname, email, password, walletId)
      addUserSession newId
      putStrLn "Аккаунт успешно создан."
      return True
