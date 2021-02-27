module Main where

import Game

main :: IO ()
main = do
  putStrLn $ replicate 10 '\n'
  putStrLn "\nWelcome to Sudoku!\n"
  putStrLn "To play a number on the board, enter in format: row(1..9) column(1..9) number(1..9) -- (ie. 1 3 4)\n"
  putStrLn "To ask for help, enter in format: row(1..9) column(1..9) -- (ie. 1 3)"
  putStrLn "Help will be given by revealing the correct number at the coordinate you enter.\n"
  putStrLn "Enter an incorrect number and you will lose a point! Lose 9 points and you will lose the game!\n"
  putStrLn "Control-C at any time to quit.\n"
  putStrLn "Starting board (size: 9x9)"
  play tBoard tSolvedBoard 