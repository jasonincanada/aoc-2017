{- Advent of Code 2017

   Day: 1 - Inverse Captcha
   URL: http://adventofcode.com/2017/day/1

   Part 1
-}

import Data.Char (digitToInt)

stream1 :: String -> [Int]
stream1 = map digitToInt

stream2 :: String -> [Int]
stream2 s = map digitToInt (drop 1 s ++ take 1 s)

reduce :: Int -> Int -> Int
reduce a b
  | a == b    = a
  | otherwise = 0

process :: String -> Int
process s = sum $ zipWith reduce (stream1 s) (stream2 s)

main = do
  file <- readFile "1.input"
  let input = (lines file) !! 0
  print $ process input
  