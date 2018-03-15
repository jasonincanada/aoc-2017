{- Advent of Code 2017

   Day: 22 - Sporifica Virus
   URL: http://adventofcode.com/2017/day/22

   Part 1
-}

import Data.List (elemIndices, (\\))
import Stackoverflow (rotate)

-- The grid is stored as a list of coordinates of infected nodes
type Grid      = [Position]
type Position  = (Int, Int)
type Cursor    = Position
type Direction = (Int, Int)

directions :: [Direction]
directions = [up, left, down, right]
  where up    = (1, 0)
        down  = (-1, 0)
        left  = (0, -1)
        right = (0, 1)

(+>) :: Position -> Direction -> Position
(+>) (row, col) (dr, dc) = (row+dr, col+dc)

next :: (Grid, Cursor, [Direction], Int, Int) -> (Grid, Cursor, [Direction], Int, Int)
next (grid, cursor, directions, infections, n) =
  if cursor `elem` grid
  then (cleaned , cursor +> head (right directions), right directions, infections  , n+1)
  else (infected, cursor +> head (left  directions), left  directions, infections+1, n+1)
    where 
      cleaned  = grid \\ [cursor]
      infected = cursor : grid
      
      right, left :: [Direction] -> [Direction]
      left  ds = rotate 1 ds
      right ds = rotate (length ds - 1) ds

process :: Grid -> Cursor -> [(Grid, Cursor, [Direction], Int, Int)]
process g c = iterate next (g, c, directions, 0, 0)

-- Convert ASCII grid into list of infected nodes
toGrid :: [String] -> [Position]
toGrid lines = concat $ go 0 lines
  where go _ []         = []
        go y (row:rows) = let positions = [ (y, x) | x <- elemIndices '#' row ]
                          in  positions : go (y-1) rows

main = do
  file <- readFile "22.input"
  let input  = lines file
  let grid   = toGrid input
  let size   = length (head input)
  let half   = size `div` 2
  let cursor = (-half, half)

  -- Iterate the state change 10,000 times and retrieve the number of new infections
  print $ last $ map       (\(_, _, _, n, _) -> n) 
               $ takeWhile (\(_, _, _, _, i) -> i <= 10000) 
               $ process grid cursor

