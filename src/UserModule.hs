{-# LANGUAGE OverloadedStrings #-}

module UserModule where

import Database.SQLite.Simple
import WalletModule

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
  users <- query db "SELECT * FROM users WHERE email = ? AND password = ? LIMIT 1;" (email, password) :: IO [User]
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
      let user = head users
      putStrLn (show user)

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

      let walletId = createWallet db
      putStrLn (show walletId)
      execute db "INSERT INTO users (name, surname, email, password, wallet) VALUES (?, ?, ?, ?, ?);" (name, surname, email, password, walletId)
