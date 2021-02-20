----------------------------------------------------------------
-- This code has been developed referencing the code for hw1
-- To compile, from /src: ghc --make game.hs
-- To run the game: ./game
----------------------------------------------------------------

module Game where

import Data.List (intercalate)
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

boardRows :: [[Index]]
boardRows = [rowIndices c | c <- coordinates]

-- List of all board indices
allIndices :: [Index]
allIndices = concat boardRows

-- The player can input a number or leave it blank
data Player = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Blank
  deriving (Eq, Show)

-- A cell can have a number put by the user or it can be empty
data Cell = Mark Player | Empty
  deriving (Eq)

-- Instance of Show to print cells
instance Show Cell where
  show (Mark a) = show a
  show Empty = " "

type Board = [[Cell]]

-- Create a list of lists of cells with all empty
emptyBoard :: [[Cell]]
emptyBoard = replicate 9 $ replicate 9 Empty

-- Print the board in a hunan readable format
showBoard :: Board -> String
showBoard b =
  let rows = map (\row -> "|" ++ (show $ row !! 0) ++ "|" ++ (show $ row !! 1) ++ "|" ++ (show $ row !! 2) ++ "|" ++ (show $ row !! 3) ++ "|" ++ (show $ row !! 4) ++ "|" ++ (show $ row !! 5) ++ "|" ++ (show $ row !! 6) ++ "|" ++ (show $ row !! 7) ++ "|" ++ (show $ row !! 8) ++ "|") b in (intercalate "\n+-----+-----+-----+\n" rows) ++ "\n"

