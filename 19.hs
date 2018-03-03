{- Advent of Code 2017

   Day: 19 - A Series of Tubes
   URL: http://adventofcode.com/2017/day/19

   Parts 1/2
-}

import Prelude hiding (Left, Right)
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

jump :: Direction -> Int -> Direction
jump d j = let i = firstIndex d directions
               i' = (i + j) `mod` 4
           in  directions !! i'

data Rotation = Left
              | Right

rotate :: Rotation -> Direction -> Direction
rotate Left  d = jump d  1
rotate Right d = jump d (-1)

data Movement = Rotate Rotation
              | Advance
              | Halt

(+>) :: Position -> Direction -> Position
(+>) (row, col) (dr, dc) = (row+dr, col+dc)

-- Determine the next valid movement on the path, trying forward, left, right
next :: Grid -> Position -> Direction -> Movement
next grid pos dir
  | isStep (id)           = Advance
  | isStep (rotate Left)  = Rotate Left
  | isStep (rotate Right) = Rotate Right
  | otherwise             = Halt

  where
    isStep f = let pos' = pos +> f dir
               in  at grid pos' /= ' '

-- Walk the path, collecting the list of positions visited and ASCII characters seen
walk :: Grid -> Position -> Direction -> ([Position], [Char])
walk grid p d = go p d [] []
  where
    go pos dir positions steps =
      case next grid pos dir of

        Advance      -> let pos'   = dir +> pos
                            step   = at grid pos'
                        in  go pos' dir (pos' : positions) (step : steps)

        Rotate Left  -> go pos (rotate Left  dir) positions steps
        Rotate Right -> go pos (rotate Right dir) positions steps

        Halt         -> (reverse positions, reverse steps)

-- Extract the letters we saw along the way
letters :: [Char] -> [Char]
letters = filter (flip elem ['A'..'Z'])

main = do
  file <- readFile "19.input"
  let grid = lines file
  let pathstart = start grid
  let (positions, chars) = walk grid pathstart (0, -1)
  print $ (length positions + 1, letters chars)

