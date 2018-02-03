{- Advent of Code 2017

   Day: 2 - Corruption Checksum
   URL: http://adventofcode.com/2017/day/2

   Part 2
-}

isDivisor :: (Int, Int) -> Bool
isDivisor nums = fst nums `mod` snd nums == 0

reduce :: [Int] -> Int
reduce nums = let count = length nums
                  tries = [ (nums !! i, nums !! j) | i <- [0..count-1], 
                                                     j <- [0..count-1],
                                                     i /= j ]
                  first = head $ filter isDivisor tries
              in  fst first `div` snd first

process :: [String] -> Int
process rows = sum $ map (reduce . map read . words) rows

main = do
  file <- readFile "2.input"
  let input = lines file
  print $ process input
