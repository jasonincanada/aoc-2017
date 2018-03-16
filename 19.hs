{- Advent of Code 2017

   Day: 19 - A Series of Tubes
   URL: http://adventofcode.com/2017/day/19

   Parts 1/2
-}

import Prelude hiding (Left, Right)
import Helpers (firstIndex)
import Stackoverflow (mmult)

type Grid = [[Char]]
type Position = (Int, Int)
type Direction = (Int, Int)
type Step = Char

-- Find the first pipe in the first row
start :: Grid -> Position
start g = let col = firstIndex '|' (head g)
          in  (0, col)

-- Get the character at a position in the grid
at :: Grid -> Position -> Step
at grid (row, col) = (grid !! row) !! col

data Rotation = Left
              | Right

rotate :: Rotation -> Direction -> Direction
rotate Left  d = rot   1  d
rotate Right d = rot (-1) d

rot :: Int -> Direction -> Direction
rot s (row, col) = let [[row'], [col']] = [[0, -s], [s, 0]] `mmult` [[row], [col]]
                   in  (row', col')

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
walk grid p d = go p d [p] []
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
letters = filter (`elem` ['A'..'Z'])

main = do
  file <- readFile "19.input"
  let grid = lines file
  let pathstart = start grid
  let (positions, chars) = walk grid pathstart (0, -1)
  print (length positions, letters chars)

