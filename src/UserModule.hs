{-# LANGUAGE OverloadedStrings #-}

module UserModule where

import Database.SQLite.Simple
import WalletModule
import SQLplotter (addUserSession)

data User = User {id :: Int, name :: String, surname :: String, email :: String, password :: String, wallet :: Int} deriving (Show)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow User where
  toRow (User id name surname email password wallet) = toRow (id, name, surname, email, password, wallet)

signIn :: IO ()
signIn = do
  dataBase <- open "local.db"
  findAccount dataBase
  close dataBase

signUp :: IO ()
signUp = do
  dataBase <- open "local.db"
  createAccount dataBase
  close dataBase

findAccount :: Connection -> IO ()
findAccount db = do
  putStrLn "Введите вашу электронную почту:"
  email <- getLine
  putStrLn "Введите ваш пароль:"
  password <- getLine
  users <- query db "SELECT id FROM users WHERE email = ? AND password = ? LIMIT 1;" (email, password) :: IO [Only Integer]
  if null users
    then do
      putStrLn "Аккаунт не найден"
      putStrLn "1. Попробовать еще раз"
      putStrLn "_. Выйти"
      choice <- getLine
      case choice of
        "1" -> findAccount db
        _ -> putStrLn "Выход..."
    else do
      let Only id = head users
      addUserSession id

createAccount :: Connection -> IO ()
createAccount db = do
  putStrLn "Введите вашу электронную почту:"
  email <- getLine
  putStrLn "Введите ваш пароль:"
  password <- getLine
  putStrLn "Повторите ваш пароль:"
  verifyPassword <- getLine

  let isPasswordVerified = password == verifyPassword
  if not isPasswordVerified
    then do
      putStrLn "Пароли не совпадают."
      putStrLn "1. Попробовать еще раз"
      putStrLn "_. Выйти"
      choice <- getLine
      case choice of
        "1" -> createAccount db
        _ -> putStrLn "Выход..."
    else do
      putStrLn "Введите ваше имя:"
      name <- getLine
      putStrLn "Введите вашу фамилию:"
      surname <- getLine

      ids <- query_ db "SELECT id FROM users ORDER BY id DESC LIMIT 1;" :: IO [Only Integer] 
      let newId  = if null ids
            then 1 
            else let Only lastId = head ids in lastId + 1 

      -- putStrLn ("Ваш id: "++show newId)
      walletId <- createWallet db
      -- putStrLn ("Ваш ID кошелька: " ++ show walletId)
      execute db "INSERT INTO users (id, name, surname, email, password, wallet) VALUES (?, ?, ?, ?, ?, ?);" (newId, name, surname, email, password, walletId)
      addUserSession newId

