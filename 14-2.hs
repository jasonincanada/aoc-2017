{- Advent of Code 2017

   Day: 14 - Disk Defragmentation
   URL: http://adventofcode.com/2017/day/14

   Part 2

   Remarks: This uses the text file generated by 14-init.hs
-}

import Data.List (intersect, union, (\\))

type Position = (Int, Int)
type Direction = (Int, Int)
type Grid = [Position]

-- Define right, up, left, down
directions :: [Direction]
directions = [(1,0), (0,1), (-1,0), (0,-1)]

add :: Position -> Direction -> Position
add (row, col) (dr, dc) = (row+dr, col+dc)

neighbours :: Grid -> Position -> [Position]
neighbours grid p = intersect grid $ map (add p) directions

-- Turn our ASCII grid into a list of coordinates of the hash signs
cells :: Int -> [String] -> Grid
cells len = map fst . filter ((=='#') . snd) . index . concat
  where
    index :: String -> [((Int, Int), Char)]
    index = zipWith3 f rows cols
    f row col x = ((row, col), x)
    rows = concat [ replicate len r | r <- [0..] ]
    cols = cycle [0..len-1]

region :: Position -> Grid -> [Position]
region p grid = go [] [p]
  where
    go found []    = found
    go found queue = let next = head queue
                         queue' = tail $ union queue $ neighbours grid next \\ found
                         found' = next : found
                     in  go found' queue' 

partition :: Grid -> [[Position]]
partition = go []
  where
    go regions []   = regions
    go regions grid = let next = head grid
                          reg = region next grid
                      in  go (reg : regions) (grid \\ reg)

process :: Grid -> Int
process = length . partition

main = do
  file <- readFile "14-grid.txt"
  let rowsize = 128
  let grid = cells rowsize $ lines file
  print $ process grid

