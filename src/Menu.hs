-- src/Menu.hs

module Menu where

import           Ads           (viewAvailableAds, filterAvailableAds)
import           Control.Monad (when)
import           UserModule    (signIn, signUp)

startMenu :: IO ()
startMenu = do
  putStrLn "Главное меню"
  putStrLn "1. Войти"
  putStrLn "2. Зарегистрироваться"
  putStrLn "3. Выход"
  putStrLn "Введите выбранный пункт меню:"

  choice <- getLine
  case choice of
    "1" -> do
      success <- signIn
      if success
        then afterAuth
        else startMenu
    "2" -> do
      signUp
      startMenu
    "3" -> putStrLn "Выход..."
    _   -> do
      putStrLn "Неверный выбор, попробуйте снова."
      startMenu

afterAuth :: IO ()
afterAuth = do
  putStrLn "\n----- Меню после аутентификации -----"
  putStrLn "1. Просмотреть доступные варианты к покупке"
  putStrLn "2. Создать объявление"
  putStrLn "3. Просмотреть свой аккаунт"
  putStrLn "4. Просмотреть свои объявления"
  putStrLn "5. Выход"
  putStrLn "Введите выбранный пункт меню:"

  choice <- getLine
  case choice of
    "1" -> do
      filterAvailableAds
      afterAuth
    "2" -> do
      -- Здесь можно добавить вызов функции для создания объявления
      putStrLn "Функциональность пока не реализована."
      afterAuth
    "3" -> do
      -- Здесь можно добавить вызов функции для просмотра аккаунта
      putStrLn "Функциональность пока не реализована."
      afterAuth
    "4" -> do
      -- Здесь можно добавить вызов функции для просмотра своих объявлений
      putStrLn "Функциональность пока не реализована."
      afterAuth
    "5" -> putStrLn "Выход..."
    _   -> do
      putStrLn "Неверный выбор, попробуйте снова."
      afterAuth
