module Game where

import Data.List (intersperse)
import Data.Ord ()
import GHC.Generics ()

-- The board is composed by 9x9 cells.
-- Each cells can be defined with an index type.
data Coordinate = C0 | C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8
  deriving (Eq, Ord, Show)

-- The index is a tuple of coordinates
type Index = (Coordinate, Coordinate)

-- List of all coordinates
coordinates :: [Coordinate]
coordinates = [C0, C1, C2, C3, C4, C5, C6, C7, C8]

-- List of coordinates Blocks.
-- These are used to check each block
-- For example block 1 is made of two coBlock1
coBlock1 :: [Coordinate]
coBlock1 = [C0, C1, C2]

coBlock2 :: [Coordinate]
coBlock2 = [C3, C4, C5]

coBlock3 :: [Coordinate]
coBlock3 = [C6, C7, C8]

-- List of all possible number options
numbers :: [Player]
numbers = [One, Two, Three, Four, Five, Six, Seven, Eight, Nine]

-- To check the game we need a list of rows and columns since for each row
-- or columns we need to have numbers from 1 to 9 in order to have a valid board
rowIndices :: Coordinate -> [Index]
rowIndices cy = [(cy, y) | y <- coordinates]

columnIndices :: Coordinate -> [Index]
columnIndices cx = [(cx, x) | x <- coordinates]

-- TODO
-- For the check we also need to control each of 3x3 blocks
-- blockIndex :: coordinates -> [Index]
-- blockIndex cx cy = undefined

-- List of lists of rows by index
boardRows :: [[Index]]
boardRows = [rowIndices c | c <- coordinates]

-- List of all board indices
allIndices :: [Index]
allIndices = concat boardRows

-- The player can input a number
data Player = One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Eq, Show)

-- A cell can have a number put by the user or it can be empty
data Cell = Mark Player | Empty
  deriving (Eq)

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

data Board = Board {cell :: Index -> Cell}

instance Show Board where
  show b =
    unlines (map (concat . intersperse " " . map (show . cell b)) boardRows)

-- Board used for testing purpose
-- Reference: https://sudoku.com/easy/
-- Each block is composed of 9 cells (3x3)
-- blocks 1 2 3
--        4 5 6
--        7 8 9
test :: Index -> Cell
test (x, y) =
  case (x, y) of
    -- Block 1
    (C0, C0) -> Mark Five
    (C0, C1) -> Mark Three
    (C0, C2) -> Empty
    (C1, C0) -> Mark Six
    (C1, C1) -> Empty
    (C1, C2) -> Empty
    (C2, C0) -> Empty
    (C2, C1) -> Mark Nine
    (C2, C2) -> Mark Eight
    -- Block 2
    (C0, C3) -> Empty
    (C0, C4) -> Mark Seven
    (C0, C5) -> Empty
    (C1, C3) -> Mark One
    (C1, C4) -> Mark Nine
    (C1, C5) -> Mark Five
    (C2, C3) -> Empty
    (C2, C4) -> Empty
    (C2, C5) -> Empty
    -- Block 3
    (C0, C6) -> Empty
    (C0, C7) -> Empty
    (C0, C8) -> Empty
    (C1, C6) -> Empty
    (C1, C7) -> Empty
    (C1, C8) -> Empty
    (C2, C6) -> Empty
    (C2, C7) -> Mark Six
    (C2, C8) -> Empty
    -- Block 4
    (C3, C0) -> Mark Eight
    (C3, C1) -> Empty
    (C3, C2) -> Empty
    (C4, C0) -> Mark Four
    (C4, C1) -> Empty
    (C4, C2) -> Empty
    (C5, C0) -> Mark Seven
    (C5, C1) -> Empty
    (C5, C2) -> Empty
    -- Block 5
    (C3, C3) -> Empty
    (C3, C4) -> Mark Six
    (C3, C5) -> Empty
    (C4, C3) -> Mark Eight
    (C4, C4) -> Empty
    (C4, C5) -> Mark Three
    (C5, C3) -> Empty
    (C5, C4) -> Mark Two
    (C5, C5) -> Empty
    -- Block 6
    (C3, C6) -> Empty
    (C3, C7) -> Empty
    (C3, C8) -> Mark Three
    (C4, C6) -> Empty
    (C4, C7) -> Empty
    (C4, C8) -> Mark One
    (C5, C6) -> Empty
    (C5, C7) -> Empty
    (C5, C8) -> Mark Six
    -- Block 7
    (C6, C0) -> Empty
    (C6, C1) -> Mark Six
    (C6, C2) -> Empty
    (C7, C0) -> Empty
    (C7, C1) -> Empty
    (C7, C2) -> Empty
    (C8, C0) -> Empty
    (C8, C1) -> Empty
    (C8, C2) -> Empty
    -- Block 8
    (C6, C3) -> Empty
    (C6, C4) -> Empty
    (C6, C5) -> Empty
    (C7, C3) -> Mark Four
    (C7, C4) -> Mark One
    (C7, C5) -> Mark Nine
    (C8, C3) -> Empty
    (C8, C4) -> Mark Eight
    (C8, C5) -> Empty
    -- Block 9
    (C6, C6) -> Mark Two
    (C6, C7) -> Mark Eight
    (C6, C8) -> Empty
    (C7, C6) -> Empty
    (C7, C7) -> Empty
    (C7, C8) -> Mark Five
    (C8, C6) -> Empty
    (C8, C7) -> Mark Seven
    (C8, C8) -> Mark Nine

testSolved :: Index -> Cell
testSolved (x, y) =
  case (x, y) of
    -- Block 1
    (C0, C0) -> Mark Five
    (C0, C1) -> Mark Three
    (C0, C2) -> Mark Four
    (C1, C0) -> Mark Six
    (C1, C1) -> Mark Seven
    (C1, C2) -> Mark Two
    (C2, C0) -> Mark One
    (C2, C1) -> Mark Nine
    (C2, C2) -> Mark Eight
    -- Block 2
    (C0, C3) -> Mark Six
    (C0, C4) -> Mark Seven
    (C0, C5) -> Mark Eight
    (C1, C3) -> Mark One
    (C1, C4) -> Mark Nine
    (C1, C5) -> Mark Five
    (C2, C3) -> Mark Three
    (C2, C4) -> Mark Four
    (C2, C5) -> Mark Two
    -- Block 3
    (C0, C6) -> Mark Nine
    (C0, C7) -> Mark One
    (C0, C8) -> Mark Two
    (C1, C6) -> Mark Three
    (C1, C7) -> Mark Four
    (C1, C8) -> Mark Eight
    (C2, C6) -> Mark Five
    (C2, C7) -> Mark Six
    (C2, C8) -> Mark Seven
    -- Block 4
    (C3, C0) -> Mark Eight
    (C3, C1) -> Mark Five
    (C3, C2) -> Mark Nine
    (C4, C0) -> Mark Four
    (C4, C1) -> Mark Two
    (C4, C2) -> Mark Six
    (C5, C0) -> Mark Seven
    (C5, C1) -> Mark One
    (C5, C2) -> Mark Three
    -- Block 5
    (C3, C3) -> Mark Seven
    (C3, C4) -> Mark Six
    (C3, C5) -> Mark One
    (C4, C3) -> Mark Eight
    (C4, C4) -> Mark Five
    (C4, C5) -> Mark Three
    (C5, C3) -> Mark Nine
    (C5, C4) -> Mark Two
    (C5, C5) -> Mark Four
    -- Block 6
    (C3, C6) -> Mark Four
    (C3, C7) -> Mark Two
    (C3, C8) -> Mark Three
    (C4, C6) -> Mark Seven
    (C4, C7) -> Mark Nine
    (C4, C8) -> Mark One
    (C5, C6) -> Mark Eight
    (C5, C7) -> Mark Five
    (C5, C8) -> Mark Six
    -- Block 7
    (C6, C0) -> Mark Nine
    (C6, C1) -> Mark Six
    (C6, C2) -> Mark One
    (C7, C0) -> Mark Two
    (C7, C1) -> Mark Eight
    (C7, C2) -> Mark Seven
    (C8, C0) -> Mark Three
    (C8, C1) -> Mark Four
    (C8, C2) -> Mark Five
    -- Block 8
    (C6, C3) -> Mark Five
    (C6, C4) -> Mark Three
    (C6, C5) -> Mark Seven
    (C7, C3) -> Mark Four
    (C7, C4) -> Mark One
    (C7, C5) -> Mark Nine
    (C8, C3) -> Mark Two
    (C8, C4) -> Mark Eight
    (C8, C5) -> Mark Six
    -- Block 9
    (C6, C6) -> Mark Two
    (C6, C7) -> Mark Eight
    (C6, C8) -> Mark Four
    (C7, C6) -> Mark Six
    (C7, C7) -> Mark Three
    (C7, C8) -> Mark Five
    (C8, C6) -> Mark One
    (C8, C7) -> Mark Seven
    (C8, C8) -> Mark Nine
 

tBoard :: Board
tBoard = Board test

tSolvedBoard :: Board
tSolvedBoard = Board testSolved

-- Check if a number is in a row
checkRow :: Board -> Player -> Coordinate -> Bool
checkRow b p cx = Mark p `elem` [cell b (cx, y) | y <- coordinates]

-- Check if all 9 numbers are in a row
checkRowForAll :: Board -> Coordinate -> Bool
checkRowForAll b cx = and [checkRow b n cx | n <- numbers]

-- Check if every row has all 9 numbers
checkAllRows :: Board -> Bool
checkAllRows b = and [checkRowForAll b cx | cx <- coordinates]

-- Check if a number is in a Column
checkColumn :: Board -> Player -> Coordinate -> Bool
checkColumn b p cy = Mark p `elem` [cell b (x, cy) | x <- coordinates]

-- Check if all 9 numbers are in a column
checkColumnForAll :: Board -> Coordinate -> Bool
checkColumnForAll b cy = and [checkRow b n cy | n <- numbers]

-- Check if every row has all 9 numbers
checkAllColumns :: Board -> Bool
checkAllColumns b = and [checkRowForAll b cy | cy <- coordinates]

-- Below are series of check blocks functions used to check the 9 blocks of
-- the Sudoku board. The board representations is as below:
-- coBlock 1 2 3
-- 1       1 2 3
-- 2       4 5 6
-- 3       7 8 9
checkBlock1 :: Board -> Player -> Bool
checkBlock1 b p = Mark p `elem` [cell b (x, y) | x <- coBlock1, y <- coBlock1]

checkBlock2 :: Board -> Player -> Bool
checkBlock2 b p = Mark p `elem` [cell b (x, y) | x <- coBlock1, y <- coBlock2]

checkBlock3 :: Board -> Player -> Bool
checkBlock3 b p = Mark p `elem` [cell b (x, y) | x <- coBlock1, y <- coBlock3]

checkBlock4 :: Board -> Player -> Bool
checkBlock4 b p = Mark p `elem` [cell b (x, y) | x <- coBlock2, y <- coBlock1]

checkBlock5 :: Board -> Player -> Bool
checkBlock5 b p = Mark p `elem` [cell b (x, y) | x <- coBlock2, y <- coBlock2]

checkBlock6 :: Board -> Player -> Bool
checkBlock6 b p = Mark p `elem` [cell b (x, y) | x <- coBlock2, y <- coBlock3]

checkBlock7 :: Board -> Player -> Bool
checkBlock7 b p = Mark p `elem` [cell b (x, y) | x <- coBlock3, y <- coBlock1]

checkBlock8 :: Board -> Player -> Bool
checkBlock8 b p = Mark p `elem` [cell b (x, y) | x <- coBlock3, y <- coBlock2]

checkBlock9 :: Board -> Player -> Bool
checkBlock9 b p = Mark p `elem` [cell b (x, y) | x <- coBlock3, y <- coBlock3]

-- Check if all 9 numbers are in block 1
checkBlock1ForAll :: Board -> Bool
checkBlock1ForAll b = and [checkBlock1 b n | n <- numbers]

-- Check if all 9 numbers are in block 2
checkBlock2ForAll :: Board -> Bool
checkBlock2ForAll b = and [checkBlock2 b n | n <- numbers]

-- Check if all 9 numbers are in block 3
checkBlock3ForAll :: Board -> Bool
checkBlock3ForAll b = and [checkBlock3 b n | n <- numbers]

-- Check if all 9 numbers are in block 4
checkBlock4ForAll :: Board -> Bool
checkBlock4ForAll b = and [checkBlock4 b n | n <- numbers]

-- Check if all 9 numbers are in block 5
checkBlock5ForAll :: Board -> Bool
checkBlock5ForAll b = and [checkBlock5 b n | n <- numbers]

-- Check if all 9 numbers are in block 6
checkBlock6ForAll :: Board -> Bool
checkBlock6ForAll b = and [checkBlock6 b n | n <- numbers]

-- Check if all 9 numbers are in block 7
checkBlock7ForAll :: Board -> Bool
checkBlock7ForAll b = and [checkBlock7 b n | n <- numbers]

-- Check if all 9 numbers are in block 8
checkBlock8ForAll :: Board -> Bool
checkBlock8ForAll b = and [checkBlock8 b n | n <- numbers]

-- Check if all 9 numbers are in block 9
checkBlock9ForAll :: Board -> Bool
checkBlock9ForAll b = and [checkBlock9 b n | n <- numbers]

-- type Board = [[Cell]]

-- -- Create a list of lists of cells with all empty
-- emptyBoard :: [[Cell]]
-- emptyBoard = replicate 9 $ replicate 9 Empty

-- -- Starting board: level easy
-- -- https://sudoku.com/easy/
-- easyBoard :: [[Cell]]
-- easyBoard =
--   [ [Empty, Mark Three, Empty, Empty, Mark Four, Empty, Empty, Empty, Mark Six],
--     [Empty, Mark Five, Empty, Empty, Mark Nine, Empty, Empty, Empty, Empty],
--     [Mark Four, Empty, Mark Six, Mark Two, Mark One, Mark Eight, Mark Seven, Mark Five, Mark Three],
--     [Empty, Empty, Empty, Mark Nine, Empty, Mark Four, Empty, Mark Five, Mark Seven],
--     [Empty, Empty, Empty, Empty, Mark Seven, Mark Two, Empty, Mark Three, Empty],
--     [Empty, Mark Two, Empty, Empty, Mark Three, Empty, Mark Six, Empty, Empty],
--     [Mark Three, Mark Seven, Empty, Mark Five, Mark Six, Empty, Mark Four, Empty, Empty],
--     [Mark Four, Mark One, Mark Five, Mark Nine, Empty, Mark Eight, Empty, Empty, Mark Three],
--     [Empty, Empty, Mark Two, Mark Three, Empty, Mark Four, Empty, Empty, Mark Five]
--   ]

-- -- Print the board in a hunan readable format
-- showBoard :: Board -> String
-- showBoard b =
--   let rows = map (\row -> "test" ++ (show $ row !! 0) ++ "test" ++ (show $ row !! 1) ++ "test" ++ (show $ row !! 2) ++ "test" ++ (show $ row !! 3) ++ "test" ++ (show $ row !! 4) ++ "test" ++ (show $ row !! 5) ++ "test" ++ (show $ row !! 6) ++ "test" ++ (show $ row !! 7) ++ "test" ++ (show $ row !! 8) ++ "test") b in (intercalate "\n+-----+-----+-----+\n" rows) ++ "\n"

-- The emptyAt function tests whether a given index on a board is empty.

-- won :: Board -> Bool
-- won b = undefined

-- strikeOut :: Bool
-- strikeOut = undefined

emptyAt :: Board -> Index -> Bool
emptyAt b i = cell b i == Empty

-- inProgress :: Board -> Bool
-- inProgress b = not (won b) || strikeOut

write :: Index -> Player -> Board -> Board
write i x b =
  Board $ \i' ->
    if i == i' && emptyAt b i then
      Mark x
    else
      cell b i'

-- I/O Player Code

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

readNum :: Char -> Maybe Player
readNum '1' = Just One
readNum '2' = Just Two
readNum '3' = Just Three
readNum '4' = Just Four
readNum '5' = Just Five
readNum '6' = Just Six
readNum '7' = Just Seven
readNum '8' = Just Eight
readNum '9' = Just Nine
readNum _ = Nothing

playerAct :: Board -> IO Board
playerAct b = do
  input <- getLine
  let tryAgain msg = putStrLn msg >> playerAct b
  case input of
    [cx, ' ', cy, ' ', number] ->
      case (readCoord cx, readCoord cy, readNum number) of
        (Just cx', Just cy', Just number') -> let i = (cx',cy') in
          if emptyAt b i then return $ write i number' b
          else tryAgain "illegal move"
        (Nothing, _, _) -> tryAgain "invalid input on first coordinate"
        (_, Nothing, _) -> tryAgain "invalid input on second coordinate"
        (_, _, Nothing) -> tryAgain "invalid input on number"
    _ -> tryAgain "invalid input"

-- exitMsg :: Board -> IO ()
-- exitMsg b = do
--  if won b then putStrLn "You win!"
--  else putStrLn "You made too many errors. Therefore, you lose!"

play :: Board -> IO ()
play b = do
  print b
  b' <- playerAct b
  print b'
