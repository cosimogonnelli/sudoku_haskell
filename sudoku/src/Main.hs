module Main where

import Data.List
import Game
import System.IO

main :: IO ()
main = do
  putStrLn $ replicate 10 '\n'
  putStrLn "\nWelcome to Sudoku!\n"
  putStrLn "To play a number on the board, enter in format: row(1..9) column(1..9) number(1..9) -- (ie. 1 3 4)\n"
  putStrLn "To ask for help, enter in format: row(1..9) column(1..9) -- (ie. 1 3)"
  putStrLn "Help will be given by revealing the correct number at the coordinate you enter.\n"
  putStrLn "Here are 9 delicious cookies. But, enter an incorrect number and you will forfeit one!"
  putStrLn "Lose all of them and you lose the game! (We might need to change this cus this is kind of brutal)\n"
  putStrLn "Control-C at any time to quit.\n"
  putStrLn "Starting board (size: 9x9)"

  -- Currently, this only reads in one game, but additional game files could easily be added
  start <- readFile "start_board.txt"
  let list1 = words start
  solved <- readFile "solved_board.txt"
  let list2 = words solved

  -- Play <cookies> <starting-board> <solved-board>
  play 9 (fillBoard allIndices (parser list1) eBoard) (fillBoard allIndices (parser list2) eBoard)

-- Read in a board
fillBoard :: [Index] -> [Cell] -> Board -> Board
fillBoard [] [] b = b
fillBoard (x : xs) (y : ys) b = fillBoard xs ys (write x y b)

-- Reads file for words and returns list of cells
parser :: [String] -> [Cell]
parser s =
  let sNum = words [if n == ',' || n == '[' || n == ']' then ' ' else n | i <- s, n <- i]
   in [readN n | n <- sNum]
