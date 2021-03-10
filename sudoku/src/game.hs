module Game where

import Data.List (sort)
import Data.Ord ()
import GHC.Generics ()

-- The board is composed by 9x9 cells.
-- Each cells can be defined with an index type of coordinates
data Coordinate = C0 | C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8
  deriving (Eq, Ord, Show)

type Index = (Coordinate, Coordinate)

-- Separates a list into list of lists of a particular length
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  let (ys, zs) = splitAt n xs
   in ys : chunks n zs

coords :: [Coordinate]
coords = [C0, C1, C2, C3, C4, C5, C6, C7, C8]

-- A block is a 3 x 3 section of numbers in the board. There are 9 blocks
-- For example, block 1 is:
-- (C0,C0) (C0, C1) (C0, C2)
-- (C1,C0) (C1, C1) (C1, C2)
-- (C2,C0) (C2, C1) (C2, C2)
coBlocks :: [[Coordinate]]
coBlocks = chunks 3 coords

-- List of lists of indices in each block
blockIndices :: [[Index]]
blockIndices = [(,) <$> coBlocks !! x <*> coBlocks !! y | x <- [0 .. 2], y <- [0 .. 2]]

-- List of lists of column indices
columnIndices :: [[Index]]
columnIndices = chunks 9 [(y, x) | x <- coords, y <- coords]

-- List of lists of row indices
rowIndices :: [[Index]]
rowIndices = chunks 9 [(x, y) | x <- coords, y <- coords]

-- List of all possible number options
numbers :: [Player]
numbers = [One, Two, Three, Four, Five, Six, Seven, Eight, Nine]

-- All indices in no particular order or partition
allIndices :: [Index]
allIndices = (,) <$> coords <*> coords

-- The player can input a valid sudoku number 1..9
data Player = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Eq, Show, Ord)

-- A cell can have a number put by the user or it can be empty
data Cell = Mark Player | Empty
  deriving (Eq, Ord)

-- Instance of Show to print cells
instance Show Cell where
  show Empty = "."
  show (Mark a)
    | a == One = "1"
    | a == Two = "2"
    | a == Three = "3"
    | a == Four = "4"
    | a == Five = "5"
    | a == Six = "6"
    | a == Seven = "7"
    | a == Eight = "8"
    | a == Nine = "9"

newtype Board = Board {cell :: Index -> Cell}

instance Show Board where
  show b =
    unlines (map (unwords . map (show . cell b)) (chunks 9 allIndices))

-- An empty board is empty at all indices
emptyBoard :: Index -> Cell
emptyBoard (x, y) = Empty

eBoard :: Board
eBoard = Board emptyBoard

-- Checks each rectangular section (row, column, or block) for each number 1..9
checkSection :: Board -> Player -> [Index] -> Bool
checkSection b n indices = Mark n `elem` [cell b i | i <- indices]

-- Checks all rows, columns, or blocks 
checkBoard :: Board -> [[Index]] -> Bool
checkBoard b indices = and [checkSection b n i | n <- numbers, i <- indices]

-- Alternative way of checking replace checkSection and checkBoard using set-theory
checkBoard2 :: Board -> [Index] -> Bool
checkBoard2 b indices =
  let x = chunks 9 [cell b i | i <- indices]
      y = foldl1 intersect' x
   in (length x == length y) && and (zipWith (==) solution (sort y))

-- Used in checkboard2
intersect' :: [Cell] -> [Cell] -> [Cell]
intersect' [] _ = []
intersect' (x : xs) l
  | x `elem` l = x : intersect' xs l
  | otherwise = intersect' xs l

-- Used in checkboard2
solution :: [Cell]
solution = [readN n | n <- ["1", "2", "3", "4", "5", "6", "7", "8", "9"]]

-- Check if a board is not full and that the player still has cookies
gameInProgress :: Int -> Board -> Bool
gameInProgress cookies b = Empty `elem` [cell b (cx, cy) | cx <- concat coBlocks, cy <- concat coBlocks] && cookies > 0

-- Return true if every row, column and block have numbers from 1 to 9 uniquely and valid sudoku solution
solve :: Board -> IO ()
solve b
  | won = putStrLn "You Won! :)"
  | not won = putStrLn "Oh no! You lost all your cookies so you lose. :( Play again and you can have some more!"
  where
    won = checkBoard b rowIndices && checkBoard b columnIndices && checkBoard b blockIndices
    --won = checkBoard2 b (concat rowIndices) && checkBoard2 b (concat columnIndices) && checkBoard2 b (concat blockIndices)

-- Checks if index is empty
emptyAt :: Board -> Index -> Bool
emptyAt b i = cell b i == Empty

-- Write a number to an index
write :: Index -> Cell -> Board -> Board
write i x b =
  Board $ \i' -> if i == i' && emptyAt b i then x else cell b i'

-- Converts user input to coordinate type
readCoord :: Char -> Maybe Coordinate
readCoord '1' = Just C0
readCoord '2' = Just C1
readCoord '3' = Just C2
readCoord '4' = Just C3
readCoord '5' = Just C4
readCoord '6' = Just C5
readCoord '7' = Just C6
readCoord '8' = Just C7
readCoord '9' = Just C8
readCoord _ = Nothing

-- Parses number from a .txt file and convert from string to cell type
readN :: String -> Cell
readN "1" = Mark One
readN "2" = Mark Two
readN "3" = Mark Three
readN "4" = Mark Four
readN "5" = Mark Five
readN "6" = Mark Six
readN "7" = Mark Seven
readN "8" = Mark Eight
readN "9" = Mark Nine
readN "0" = Empty

-- Converts user input to cell type
readNum :: Char -> Maybe Cell
readNum '1' = Just (Mark One)
readNum '2' = Just (Mark Two)
readNum '3' = Just (Mark Three)
readNum '4' = Just (Mark Four)
readNum '5' = Just (Mark Five)
readNum '6' = Just (Mark Six)
readNum '7' = Just (Mark Seven)
readNum '8' = Just (Mark Eight)
readNum '9' = Just (Mark Nine)
readNum _ = Nothing

-- Handles player input which can either be a play or asking for a hint
playerAct :: Int -> Board -> Board -> IO (Board, Int)
playerAct cookies b sb = do
  input <- getLine
  let tryAgain msg = putStrLn msg >> playerAct cookies b sb
  case input of
    [cx, ' ', cy, ' ', number] ->
      case (readCoord cx, readCoord cy, readNum number) of
        (Just cx', Just cy', Just number') ->
          let i = (cx', cy')
           in if emptyAt b i
                then
                  if number' == cell sb i
                    then return (write i number' b, cookies)
                    else putStrLn "That number doesn't belong at that coordinate. You lose a cookie :(" >> return (b, cookies - 1)
                else putStrLn "There is already a number at this coordinate." >> return (b, cookies)
        (Nothing, _, _) -> tryAgain "Invalid input on first coordinate for row. Must be a number 1..9"
        (_, Nothing, _) -> tryAgain "Invalid input on second coordinate for column. Must be a number 1..9"
        (_, _, Nothing) -> tryAgain "Invalid input on number. Must be a number 1..9"
    [cx, ' ', cy] ->
      case (readCoord cx, readCoord cy) of
        (Just cx', Just cy') ->
          let i = (cx', cy')
           in if emptyAt b i
                then return (write i (cell sb i) b, cookies)
                else putStrLn "There is already a number at this coordinate." >> return (b, cookies)
        (Nothing, _) -> tryAgain "Invalid input on first coordinate for row. Must be a number 1..9"
        (_, Nothing) -> tryAgain "Invalid input on second coordinate for column. Must be a number 1..9"
    _ -> tryAgain "Invalid input. To play: row(1..9) column(1..9) number(1..9) || For help: row(1..9) column(1..9)"

-- Parameter b is player's board, parameter sb is the solution board
play :: Int -> Board -> Board -> IO ()
play cookies b sb = do
  print b
  putStrLn $ unwords (replicate cookies "🍪")
  if gameInProgress cookies b
    then do
      putStrLn "To play: row(1..9) column(1..9) number(1..9) || For help: row(1..9) column(1..9) || To quit: ctrl-c: "
      (b', cookies') <- playerAct cookies b sb
      putStrLn ""
      if gameInProgress cookies' b' then play cookies' b' sb else solve b'
    else solve b
