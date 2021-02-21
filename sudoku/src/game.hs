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
-- Block 1
test (x, y) =
  case (x, y) of
    (C0, C0) -> Empty
    (C0, C1) -> Mark Three
    (C0, C2) -> Empty
    (C1, C0) -> Empty
    (C1, C1) -> Mark Four
    (C1, C2) -> Empty
    (C2, C0) -> Empty
    (C2, C1) -> Empty
    (C2, C2) -> Mark Six
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

showTestingBoard :: Board
showTestingBoard = Board test

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
