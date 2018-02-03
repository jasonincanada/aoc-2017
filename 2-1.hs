{- Advent of Code 2017

   Day: 2 - Corruption Checksum
   URL: http://adventofcode.com/2017/day/2

   Part 1
-}

reduce :: [Int] -> Int
reduce nums = maximum nums - minimum nums

process :: [String] -> Int
process rows = sum $ map (reduce . map read . words) rows

main = do
  file <- readFile "2.input"
  let input = lines file
  print $ process input
