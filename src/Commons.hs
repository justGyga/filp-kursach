module Commons where

import System.Process (system)

clearCLI :: IO ()
clearCLI = do
  _ <- system "cls"
  putStrLn ""

getNumericInput :: (Read a, Ord a, Num a) => String -> (a -> Bool) -> IO (Maybe a)
getNumericInput prompt validator = do
  putStrLn prompt
  input <- getLine
  case reads input of
    [(value, "")] | validator value -> return (Just value)
    _ -> do
      putStrLn "Некорректное значение. Попробуйте еще раз."
      putStrLn "1. Повторить ввод"
      putStrLn "_. Пропустить"
      choice <- getLine
      if choice == "1"
        then getNumericInput prompt validator
        else return Nothing

getInteger :: String -> Integer -> IO Integer
getInteger prompt minValue = do
  putStrLn prompt
  input <- getLine
  case reads input of
    [(n, "")] ->
      if n >= minValue
        then return n
        else do
          putStrLn $ "Пожалуйста, введите число больше или равное " ++ show minValue
          getInteger prompt minValue
    _ -> do
      putStrLn "Введенное значение не является числом. Пожалуйста, введите корректное целое число"
      getInteger prompt minValue

getFloat :: String -> IO Float
getFloat prompt = do
  putStrLn prompt
  input <- getLine
  case reads input of
    [(n, "")] ->
      if n > 0
        then return n
        else do
          putStrLn "Пожалуйста, введите положительное число"
          getFloat prompt
    _ -> do
      putStrLn "Пожалуйста, введите корректное число"
      getFloat prompt

getString :: String -> Bool -> IO String
getString prompt allowEmpty = do
  putStrLn prompt
  str <- getLine
  if null str && not allowEmpty
    then do
      putStrLn "Строка не может быть пустой. Попробуйте еще раз."
      getString prompt allowEmpty
    else return str

getBoolean :: String -> IO Bool
getBoolean prompt = do
  putStrLn prompt
  putStrLn "    y/Y - да, любое другое значение - нет"
  input <- getLine
  return (input == "y" || input == "Y")

getEmail :: String -> IO String
getEmail prompt = do
  putStrLn prompt
  input <- getLine
  if isValidEmail input
    then return input
    else do
      putStrLn "Некорректный email адрес. Пожалуйста, введите корректный email."
      getEmail prompt

isValidEmail :: String -> Bool
isValidEmail email = 
  let hasAt = '@' `elem` email
      parts = words [if c == '@' then ' ' else c | c <- email]
      validChars = all (\c -> c `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ".-_@")) email
      hasLocalPart = not (null (head parts))
      hasDomain = length parts == 2 && not (null (last parts)) && '.' `elem` last parts
  in hasAt && validChars && hasLocalPart && hasDomain
  
