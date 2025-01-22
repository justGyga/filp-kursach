module Main where

import Commons (clearCLI)
import Menu (startMenu)
import SQLplotter (addSessionTable, initializeDB, seedDB)

main :: IO ()
main = do
  -- initializeDB
  -- seedDB
  addSessionTable
  clearCLI
  startMenu
