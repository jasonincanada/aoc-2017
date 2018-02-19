{- Advent of Code 2017

   Day: 11 - Hex Ed
   URL: http://adventofcode.com/2017/day/11

   Good resource for understanding hex grids: https://www.redblobgames.com/grids/hexagons/#distances

   Part 1/2
-}

type Cell = (Int, Int, Int)

add :: Cell -> Cell -> Cell
add (x1, y1, z1) (x2, y2, z2) = (x1+x2, y1+y2, z1+z2)

step :: String -> Cell
step "n"  = ( 0,  1, -1)
step "s"  = ( 0, -1,  1)
step "nw" = (-1,  1,  0)
step "se" = ( 1, -1,  0)
step "ne" = ( 1,  0, -1)
step "sw" = (-1,  0,  1)

distance :: Cell -> Int
distance (x, y, z) = (abs x + abs y + abs z) `div` 2

process :: [String] -> (Cell, Int, Int)
process s = let steps = scanl add (0, 0, 0) (map step s)
                final = last steps
                dist = distance final
                max = maximum $ map distance steps
            in  (final, dist, max)

removecommas :: String -> String
removecommas = map (\c -> if c == ',' then ' ' else c)

main = do
  file <- readFile "11.input"
  let input = head $ lines file
  let steps = words $ removecommas input
  print $ process steps
  
