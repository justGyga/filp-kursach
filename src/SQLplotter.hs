module SQLplotter where

import Data.String (fromString)
import Data.Text (Text)
import Database.SQLite.Simple

connectToDB :: IO Connection
connectToDB = open "app.db"
