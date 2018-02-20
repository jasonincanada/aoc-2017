{- Advent of Code 2017

   Day: 12 - Digital Plumber
   URL: http://adventofcode.com/2017/day/12

   Part 1

       [0] [82]
   82:   
-}

import Data.List (union, (\\))

programcount :: [[Int]] -> Int -> Int
programcount ps i = f [i] (ps !! i) 
  where
    f known []     = length known
    f known queue  = let next = head queue
                         queue' = tail $ union queue $ (ps !! next) \\ known
                         known' = next : known 
                     in  f known' queue'

removecommas :: String -> String
removecommas = map (\c -> if c == ',' then ' ' else c)

main = do
  file <- readFile "12.input"
  let input = lines file
  let pipes = map (map read . drop 2 . words . removecommas) input
  print $ programcount pipes 0
  
