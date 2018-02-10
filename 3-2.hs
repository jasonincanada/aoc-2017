{- Advent of Code 2017

   Day: 3 - Spiral Memory
   URL: http://adventofcode.com/2017/day/3

   Part 2
-}

import Data.List

type Position = (Int, Int)
type Step = (Int, Int)
type Board = [(Position, Int)]

toNext :: [Step]
toNext = [(1,0)]

directions :: [Step]
directions = [(0,1), (-1,0), (0,-1), (1,0)]

-- Steps to traverse one layer/ring of the board
steps :: Int -> [Step]
steps 0 = []
steps n = concat $ map (replicate n) directions

-- Infinite list of steps expanding outwards from center
path :: [Step]
path = toNext ++ intercalate toNext [ tail $ steps n | n <- [2,4..] ]

add :: Position -> Step -> Position
add (px, py) (dx, dy) = (px+dx, py+dy)

-- Coordinates of the nth element in the spiral
pos :: Int -> Position
pos n = last $ scanl add (0,0) $ take (n-1) path

-- Relative coordinates for the surrounding squares
around :: [Step]
around = [ (x, y) | x <- [-1, 0, 1],
                    y <- [-1, 0, 1],
                    not (x == 0 && y == 0) ]

next :: Board -> Board
next [] = [((0,0), 1)]
next board = let coords = pos $ length board + 1
                 adjacents = [ add coords a | a <- around ]
                 newsum = sum $ map (\(p, val) -> if p `elem` adjacents then val else 0) board
             in  board ++ [(coords, newsum)]

-- Get the nth board
board :: Int -> Board
board n = go n []
  where go n bs = if n == 0 then bs else go (n-1) (next bs)

main = do
  print $ last $ board 65

