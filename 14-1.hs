{- Advent of Code 2017

   Day: 14 - Disk Defragmentation
   URL: http://adventofcode.com/2017/day/14

   Part 1

   Remarks: This uses the text file generated from the output of 14-init.hs
-}

process :: [String] -> Int
process lines = length $ filter (=='#') $ concat lines

main = do
  file <- readFile "14-grid.txt"
  let input = lines file
  print $ process input

