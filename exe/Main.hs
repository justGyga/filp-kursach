module Main where

import Commons (clearCLI)
import Menu (startMenu)
import SQLplotter (initializeDB, seedDB, addSessionTable)

main :: IO ()
main = do
  initializeDB
  seedDB
  addSessionTable
  clearCLI
  startMenu

--   putStrLn "Hello, CLI!"
