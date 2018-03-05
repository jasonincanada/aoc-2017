{- Advent of Code 2017

   Day: 21 - Fractal Art
   URL: http://adventofcode.com/2017/day/21

   Parts 1/2
-}

import Data.List (transpose, nub, lookup) 
import Data.List.Split (splitOn, chunksOf)

type Pattern = [String]
type Grid = [[Pattern]]
type Enhancement = (Pattern, Pattern)

mirror, rotate :: Pattern -> Pattern
mirror = map reverse
rotate = map reverse . transpose

-- Build the list of distinct symmetries for this pattern
rotations :: Pattern -> [Pattern]
rotations p = nub (rotates ++ flips)
  where rotates = take 4 $ iterate rotate p
        flips   = take 4 $ iterate rotate (mirror p)

parse :: String -> Enhancement
parse s = (splitOn "/" left, splitOn "/" right)
  where left  = ws !! 0
        right = ws !! 2
        ws    = words s

-- Join a grid of patterns into one pattern
join :: Grid -> Pattern
join = concat . map (foldr f empty)
  where f     = zipWith (++)
        empty = cycle [[]]

-- Split a pattern into a grid of n-by-n patterns
subdivide :: Int -> Pattern -> Grid
subdivide n p = let rows = chunksOf n p
                in  map (transpose . map (chunksOf n)) rows
            
-- Count the number of hashes in the pattern
count :: Pattern -> Int
count = sum . map (length . filter (=='#'))

-- Get the side length of a pattern
size :: Pattern -> Int
size n = length (n !! 0)

process :: [Enhancement] -> Pattern -> Pattern
process es pat = let n    = size pat
                     div  = if n `mod` 2 == 0
                            then 2
                            else 3
                     grid = subdivide div pat
                 in  join $ map (map replace) grid
  where 
    -- Look up a pattern by its various symmetries
    replace :: Pattern -> Pattern
    replace p = let rs    = rotations p
                    tries = [ lookup try es | try <- rs ]
                in  firstJust tries

    -- Extract the first match or throw an exception if none found
    firstJust :: [Maybe Pattern] -> Pattern
    firstJust []     = error "No replacement pattern found"
    firstJust (p:ps) = case p of
                         Just pat -> pat
                         Nothing -> firstJust ps

start :: Pattern
start = [".#.", "..#", "###"]

main = do
  file <- readFile "21.input"
  let enhancements = map parse $ lines file
  let iterations = take (18+1) $ iterate (process enhancements) start
  print $ (count (iterations !! 5), count (iterations !! 18))

