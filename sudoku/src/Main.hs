module Main where

import Data.List
import Game
import System.IO

main :: IO ()
main = do
  putStrLn $ take 10 $ repeat '\n'
  putStrLn "\nWelcome to Sudoku!\n"
  putStrLn "To enter a number, Use this format: rowNum columnNum sudokuNum (ie. 1 3 4).\n"
  putStrLn "To aks for help, Use this format: <help>."
  putStrLn "The hint will be given in the same format used for entering a number.\n"
  putStrLn "Type 'exit' at any time to quit.\n"
  putStrLn "Starting board (size: 9x9)"

  -- play tBoard

  -- Read in a file of boards
  -- readBoard :: FilePath -> IO ()
  -- readBoard f = do
  contents <- readFile "boards.txt"
  let items = words contents
      sudokuNum = words [if n == ',' || n == '[' || n == ']' then ' ' else n | sw <- items, n <- sw]
      listOfCellNum = [readN n | n <- sudokuNum]
      cboard = concat boardRows
  print listOfCellNum
  print cboard

-- Read in a board
fillBoard :: [Index] -> [Cell] -> Board -> Board
fillBoard [] [] b = b
fillBoard (x : xs) (y : ys) b = fillBoard xs ys (write2 x y b)
