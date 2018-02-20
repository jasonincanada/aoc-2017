{- Advent of Code 2017

   Day: 12 - Digital Plumber
   URL: http://adventofcode.com/2017/day/12

   Part 2
-}

import Data.List (union, (\\))

programs :: [[Int]] -> Int -> [Int]
programs ps i = go [i] (ps !! i) 
  where
    go known []    = known
    go known queue = let next = head queue
                         queue' = tail $ union queue $ (ps !! next) \\ known
                         known' = next : known 
                     in  go known' queue'

groupcount :: [[Int]] -> Int
groupcount ps = go [0..(length ps-1)] 0
  where
    go []    n = n
    go queue n = let progs = programs ps $ head queue
                     rest = queue \\ progs
                 in  go rest (n+1)

removecommas :: String -> String
removecommas = map (\c -> if c == ',' then ' ' else c)

main = do
  file <- readFile "12.input"
  let input = lines file
  let pipes = map (map read . drop 2 . words . removecommas) input
  print $ groupcount pipes
  
