module Main where

import Game

main :: IO ()
main = do
  putStrLn $ take 10 $ repeat '\n'
  putStrLn "\nWelcome to Sudoku!\n"
  putStrLn "To enter a number, Use this format: rowNum columnNum sudokuNum (ie. 1 3 4).\n"
  putStrLn "To aks for help, Use this format: <help>."
  putStrLn "The hint will be given in the same format used for entering a number.\n"
  putStrLn "Type 'exit' at any time to quit.\n"
  putStrLn "Starting board (size: 9x9)"
  play tBoard

-- putStrLn $ showBoard emptyBoard

-- putStrLn $ showBoard easyBoard