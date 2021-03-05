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

coBlocks :: [[Coordinate]]
coBlocks = chunks 3 coords

blockIndices :: [[Index]]
blockIndices = [(,) <$> coBlocks !! x <*> coBlocks !! y | x <- [0 .. 2], y <- [0 .. 2]]

columnIndices :: [[Index]]
columnIndices = chunks 9 [(y, x) | x <- coords, y <- coords]

rowIndices :: [[Index]]
rowIndices = chunks 9 [(x, y) | x <- coords, y <- coords]

-- List of all possible number options
numbers :: [Player]
numbers = [One, Two, Three, Four, Five, Six, Seven, Eight, Nine]

allIndices :: [Index]
allIndices = (,) <$> coords <*> coords

-- The player can input a number
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

--TODO: board left only for testing checking solution to be REMOVED
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

-- TODO: to be REMOVED
tSolvedBoard :: Board
tSolvedBoard = Board testSolved

emptyBoard :: Index -> Cell
emptyBoard (x, y) = Empty

eBoard :: Board
eBoard = Board emptyBoard

checkSection :: Board -> Player -> [Index] -> Bool
checkSection b n indices = Mark n `elem` [cell b i | i <- indices]

checkBoard :: Board -> [[Index]] -> Bool
checkBoard b indices = and [checkSection tSolvedBoard n i | n <- numbers, i <- indices]

-- Alternative way of checking replace checkSection and checkBoard
checkBoard2 :: Board -> [Index] -> Bool
checkBoard2 b indices =
  let x = chunks 9 [cell b i | i <- indices]
      y = foldl1 intersect' x
   in (length x == length y) && and (zipWith (==) solution (sort y))

intersect' :: [Cell] -> [Cell] -> [Cell]
intersect' [] _ = []
intersect' (x : xs) l
  | elem x l = x : intersect' xs l
  | otherwise = intersect' xs l

solution :: [Cell]
solution = [readN n | n <- ["1", "2", "3", "4", "5", "6", "7", "8", "9"]]

-- Check if a board is not full
gameInProgress :: Int -> Board -> Bool
gameInProgress cookies b = Empty `elem` [cell b (cx, cy) | cx <- concat coBlocks, cy <- concat coBlocks] && cookies > 0

-- Return true if every row, column and block have numbers from 1 to 9
solve :: Board -> IO ()
solve b
  | won = putStrLn "You Won! :)"
  | not won = putStrLn "Oh no! You lost all your cookies so you lose. :( Play again and you can have some more!"
  where
    -- won = checkBoard b rowIndices && checkBoard b columnIndices && checkBoard b blockIndices
    won = checkBoard2 b (concat rowIndices) && checkBoard2 b (concat columnIndices) && checkBoard2 b (concat blockIndices)

emptyAt :: Board -> Index -> Bool
emptyAt b i = cell b i == Empty

write :: Index -> Cell -> Board -> Board
write i x b =
  Board $ \i' -> if i == i' && emptyAt b i then x else cell b i'

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

-- parameter b is player's board, parameter sb is the solution board
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
