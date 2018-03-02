{- Advent of Code 2017

   Day: 19 - A Series of Tubes
   URL: http://adventofcode.com/2017/day/19

   Parts 1/2
-}

import Helpers (firstIndex)

type Grid = [[Char]]
type Position = (Int, Int)
type Direction = (Int, Int)
type Step = Char

-- Find the first pipe in the first row
start :: Grid -> Position
start g = let col = firstIndex '|' (g !! 0)
          in  (0, col)

-- Get the character at a position in the grid
at :: Grid -> Position -> Step
at grid (row, col) = (grid !! row) !! col

directions :: [Direction]
directions = [(1,0), (0,1), (-1,0), (0,-1)]

left :: Direction -> Direction
left dir = let i  = firstIndex dir directions
               i' = (i + 1) `mod` 4 
           in  directions !! i'

right :: Direction -> Direction
right dir = let i  = firstIndex dir directions
                i' = (i - 1) `mod` 4 
            in  directions !! i'

(+>) :: Position -> Direction -> Position
(+>) (row, col) (dr, dc) = (row+dr, col+dc)

-- Determine the next valid step on the path, trying forward, left, right
next :: Grid -> Position -> Direction -> Maybe (Position, Direction)
next grid pos dir
  | try id    = Just (pos +> dir, dir)
  | try left  = Just (pos +> left dir, left dir)
  | try right = Just (pos +> right dir, right dir)
  | otherwise = Nothing
    where 
      try k  = isStep $ at grid (pos +> k dir)
      isStep = (/=' ')

walk :: Grid -> Position -> Direction -> [(Position, Step)]
walk grid pos dir = go pos dir
  where
    go p d = 
      case next grid p d of
        Just (pos', dir') -> (pos', at grid pos') 
                             : go pos' dir'
        Nothing           -> []

-- Extract the letters we saw along the way
letters :: [(Position, Step)] -> [Char]
letters = filter (flip elem ['A'..'Z']) . map snd

main = do
  file <- readFile "19.input"
  let grid = lines file
  let pathstart = start grid
  let path = walk grid pathstart (0, -1)
  print $ (letters path, length path + 1)

