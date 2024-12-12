module Menu where

import UserModule (signIn, signUp)

startMenu :: IO ()
startMenu = do
  putStrLn "Главное меню"
  putStrLn "1. Войти"
  putStrLn "2. Зарегистрироваться"
  putStrLn "3. Выход"
  putStrLn "Введите выбранный пункт меню:"

  choice <- getLine
  case choice of
    "1" -> signIn
    "2" -> signUp
    "3" -> putStrLn "Выход..."
    _ -> putStrLn "Неверный выбор, попробуйте снова." >> startMenu

afterAuth :: IO ()
afterAuth = do
  putStrLn "Меню:"
  putStrLn "1. Просмотреть доступные варианты к покупке"
  putStrLn "2. Создать объявление"
  putStrLn "3. Просмотреть свой аккаунт"
  putStrLn "4. Постмотреть свои объявления"
  putStrLn "5. Выход"
  putStrLn "Введите выбранный пункт меню:"

  choice <- getLine
  case choice of
    "1" -> signIn
    "2" -> signUp
    "3" -> signUp
    "4" -> signUp
    "5" -> putStrLn "Выход..."
    _ -> putStrLn "Неверный выбор, попробуйте снова." >> startMenu
