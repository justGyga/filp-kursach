{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use when" #-}
module DatabaseInit where

import Data.String (fromString)
import Database.SQLite.Simple
import System.Directory (doesFileExist, removeFile)

initializeDB :: IO ()
initializeDB = do
  dbExists <- doesFileExist "app.db"

  if dbExists
    then do
      removeFile "app.db"
    else return ()
