{- Advent of Code 2017

   Day: 22 - Sporifica Virus
   URL: http://adventofcode.com/2017/day/22

   Part 2
-}

import qualified Data.Map as Map
import Prelude hiding (Left, Right)
import Data.List (elemIndices)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import Stackoverflow (mmult)

type Grid      = Map.Map Position Status
type Position  = (Int, Int)
type Direction = (Int, Int)

data Status = Clean
            | Weakened
            | Infected
            | Flagged
            deriving (Enum, Eq)

evolve :: Status -> Status
evolve Flagged = Clean
evolve s       = succ s

data Rotation = Forward
              | Left
              | Right
              | TurnAround

rotation :: Status -> Rotation
rotation Clean    = Left
rotation Weakened = Forward
rotation Infected = Right
rotation Flagged  = TurnAround

rotate :: Rotation -> Direction -> Direction
rotate Forward    d = d
rotate Left       d = rot (-1) d
rotate Right      d = rot 1 d
rotate TurnAround d = rot 1 $ rot 1 d

rot :: Int -> Direction -> Direction
rot s (row, col) = let [[row'], [col']] = [[0, -s], [s, 0]] `mmult` [[row], [col]]
                   in  (row', col')

advance :: Position -> Grid -> Grid
advance c grid = case Map.lookup c grid of
  Nothing      -> Map.insert c (evolve Clean) grid
  Just Flagged -> Map.delete c grid
  Just status  -> Map.insert c (evolve status) grid

get :: Position -> Grid -> Status
get c grid = fromMaybe Clean (Map.lookup c grid)

next :: (Grid, Position, Direction, Int, Int) -> (Grid, Position, Direction, Int, Int)
next (grid, cursor, dir, infects, n) =
  let status  = get cursor grid
      rot'    = rotation status
      dir'    = rotate rot' dir
      cursor' = cursor +> dir'
      grid'   = advance cursor grid
      inf'    = infects + infections status
  in  (grid', cursor', dir', inf', n + 1)

(+>) :: Position -> Direction -> Position
(+>) (row, col) (dr, dc) = (row+dr, col+dc)

infections :: Status -> Int
infections Weakened = 1
infections _        = 0

process :: Grid -> Position -> [(Grid, Position, Direction, Int, Int)]
process g c = iterate next (g, c, (1, 0), 0, 0)

-- Convert ASCII grid into list of infected nodes
toGrid :: [String] -> Grid
toGrid lines = Map.fromList $ concat $ go 0 lines
  where go _ []         = []
        go y (row:rows) = let positions = [ ((y, x), Infected) | x <- elemIndices '#' row ]
                          in  positions : go (y-1) rows

main = do
  file <- readFile "22.input"
  let input  = lines file
  args <- getArgs
  let iters  = read $ head args
  let grid   = toGrid input
  let size   = length (head input)
  let half   = size `div` 2
  let cursor = (-half, half)

  print $ last $ map       (\(_, _, _, n, _) -> n) 
               $ takeWhile (\(_, _, _, _, i) -> i <= iters) 
               $ process grid cursor

