module Main where

import Data.List
import Game
import System.IO

listOfCellNum :: [a]
listOfCellNum = []

main :: IO ()
main = do
  rf <- readFile "start_board.txt"
  let items = words rf
  let sudokuNum = words [if n == ',' || n == '[' || n == ']' then ' ' else n | sw <- items, n <- sw]
  let allN1 = [readN n | n <- sudokuNum]
  rf <- readFile "solved_board.txt"
  let items = words rf
  let sudokuNum = words [if n == ',' || n == '[' || n == ']' then ' ' else n | sw <- items, n <- sw]
  let allN2 = [readN n | n <- sudokuNum]

  putStrLn $ replicate 10 '\n'
  putStrLn "\nWelcome to Sudoku!\n"
  putStrLn "To play a number on the board, enter in format: row(1..9) column(1..9) number(1..9) -- (ie. 1 3 4)\n"
  putStrLn "To ask for help, enter in format: row(1..9) column(1..9) -- (ie. 1 3)"
  putStrLn "Help will be given by revealing the correct number at the coordinate you enter.\n"
  putStrLn "Here are 9 delicious cookies. But, enter an incorrect number and you will forfeit one!"
  putStrLn "Lose all of them and you lose the game! (We might need to change this cus this is kind of brutal)\n"
  putStrLn "Control-C at any time to quit.\n"
  putStrLn "Starting board (size: 9x9)"

  -- Play <cookies> <starting-board> <solved-board>
  play 9 (fillBoard allIndices allN1 eBoard) (fillBoard allIndices allN2 eBoard)