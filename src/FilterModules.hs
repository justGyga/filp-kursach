-- {-# LANGUAGE OverloadedStrings #-}

-- import Database.SQLite.Simple
-- import Database.SQLite.Simple.FromRow
-- import Data.Text (Text)

-- data Property = Property
--   { propId :: Int
--   , objectType :: Text
--   , area :: Float
--   , cost :: Float
--   , description :: Text
--   } deriving (Show)

-- instance FromRow Property where
--   fromRow = Property <$> field <*> field <*> field <*> field <*> field

-- -- Функция для генерации динамического SQL-запроса
-- generateQuery :: [(Text, Text)] -> (Query, [Text])
-- generateQuery filters =
--   let baseQuery = "SELECT ads.id, ads.objectType, flats.area, ads.cost, ads.description \
--                   \FROM ads \
--                   \LEFT JOIN flats ON ads.objectId = flats.id \
--                   \WHERE 1=1"
--       conditions = map (\(key, _) -> key <> " = ?") filters
--       queryWithFilters = baseQuery <> " AND " <> mconcat (map (<> " AND ") conditions)
--   in (Query queryWithFilters, map snd filters)

-- -- Функция для получения результатов на основе фильтров
-- fetchProperties :: Connection -> [(Text, Text)] -> IO [Property]
-- fetchProperties conn filters = do
--   let (query, params) = generateQuery filters
--   query conn query params

-- -- Основная функция, принимающая фильтры с консоли
-- searchProperties :: IO ()
-- searchProperties = do
--   putStrLn "Введите фильтры в формате ключ:значение (например, roomCount:3). Для завершения введите 'end':"
--   filters <- collectFilters []
--   conn <- open "local.db"
--   results <- fetchProperties conn filters
--   mapM_ print results
--   close conn
--   where
--     collectFilters acc = do
--       input <- getLine
--       if input == "end"
--         then return acc
--         else case span (/= ':') input of
--           (key, ':':value) -> collectFilters ((key, value) : acc)
--           _ -> do
--             putStrLn "Неверный формат. Попробуйте еще раз:"
--             collectFilters acc
