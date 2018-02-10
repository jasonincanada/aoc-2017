{- Advent of Code 2017

   Day: 4 - High-Entropy Passphrases
   URL: http://adventofcode.com/2017/day/4

   Part 2
-}

import Data.List

unique :: String -> [String]
unique = map head . group . sort . map sort . words 

isValid :: String -> Bool
isValid s = let all = words s
                uniq = unique s
            in  length all == length uniq

main = do
  file <- readFile "4.input"
  let ls = lines file
  print $ length $ filter isValid ls

