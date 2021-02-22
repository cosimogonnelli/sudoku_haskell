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
    -- TODO Block 1 used for testing to be removed when testing the game
    (C0, C0) -> Mark One
    (C0, C1) -> Mark Two
    (C0, C2) -> Mark Three
    (C1, C0) -> Mark Five
    (C1, C1) -> Mark Four
    (C1, C2) -> Mark Seven
    (C2, C0) -> Mark Eight
    (C2, C1) -> Mark Nine
    (C2, C2) -> Mark Six
    -- Block 1
    -- (C0, C0) -> Empty
    -- (C0, C1) -> Mark Three
    -- (C0, C2) -> Empty
    -- (C1, C0) -> Empty
    -- (C1, C1) -> Mark Four
    -- (C1, C2) -> Empty
    -- (C2, C0) -> Empty
    -- (C2, C1) -> Empty
    -- (C2, C2) -> Mark Six
    -- Block 2
    (C0, C3) -> Empty
    (C0, C4) -> Mark Five
    (C0, C5) -> Empty
    (C1, C3) -> Empty
    (C1, C4) -> Mark Nine
    (C1, C5) -> Empty
    (C2, C3) -> Empty
    (C2, C4) -> Empty
    (C2, C5) -> Empty
    -- Block 3
    (C0, C6) -> Mark Four
    (C0, C7) -> Empty
    (C0, C8) -> Mark Six
    (C1, C6) -> Mark Two
    (C1, C7) -> Mark One
    (C1, C8) -> Mark Eight
    (C2, C6) -> Mark Seven
    (C2, C7) -> Mark Five
    (C2, C8) -> Mark Three
    -- Block 4
    (C3, C0) -> Empty
    (C3, C1) -> Empty
    (C3, C2) -> Empty
    (C4, C0) -> Mark Nine
    (C4, C1) -> Empty
    (C4, C2) -> Mark Four
    (C5, C0) -> Empty
    (C5, C1) -> Mark Five
    (C5, C2) -> Mark Seven
    -- Block 5
    (C3, C3) -> Empty
    (C3, C4) -> Empty
    (C3, C5) -> Empty
    (C4, C3) -> Empty
    (C4, C4) -> Mark Seven
    (C4, C5) -> Mark Two
    (C5, C3) -> Empty
    (C5, C4) -> Mark Three
    (C5, C5) -> Empty
    -- Block 6
    (C3, C6) -> Empty
    (C3, C7) -> Mark Two
    (C3, C8) -> Empty
    (C4, C6) -> Empty
    (C4, C7) -> Mark Three
    (C4, C8) -> Empty
    (C5, C6) -> Mark Six
    (C5, C7) -> Empty
    (C5, C8) -> Empty
    -- Block 7
    (C6, C0) -> Mark Three
    (C6, C1) -> Mark Seven
    (C6, C2) -> Empty
    (C7, C0) -> Mark Five
    (C7, C1) -> Mark Six
    (C7, C2) -> Empty
    (C8, C0) -> Mark Four
    (C8, C1) -> Empty
    (C8, C2) -> Empty
    -- Block 8
    (C6, C3) -> Mark Four
    (C6, C4) -> Mark One
    (C6, C5) -> Mark Five
    (C7, C3) -> Mark Nine
    (C7, C4) -> Empty
    (C7, C5) -> Mark Eight
    (C8, C3) -> Empty
    (C8, C4) -> Empty
    (C8, C5) -> Mark Three
    -- Block 9
    (C6, C6) -> Empty
    (C6, C7) -> Empty
    (C6, C8) -> Mark Two
    (C7, C6) -> Mark Three
    (C7, C7) -> Empty
    (C7, C8) -> Mark Four
    (C8, C6) -> Empty
    (C8, C7) -> Empty
    (C8, C8) -> Mark Five

tBoard :: Board
tBoard = Board test

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
