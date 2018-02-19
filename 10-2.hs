{- Advent of Code 2017

   Day: 10 - Knot Hash
   URL: http://adventofcode.com/2017/day/10

   Part 2
-}

import Data.Char (ord)
import Data.Bits (xor)

-- From https://stackoverflow.com/questions/16378773/rotate-a-list-in-haskell
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

rev :: [a] -> Int -> Int -> [a]
rev s i n = let rot = rotate i s
                seg = take n rot
                rev = reverse seg
                new = rev ++ drop n rot
                len = length s
            in  rotate (len-i) new

process :: [a] -> [Int] -> ([a], Int, Int)
process a lengths = foldl f (a, 0, 0) (concat $ replicate 64 lengths)
  where
    f (list, i, skip) len = (rev list i len,
                             (i + skip + len) `mod` (length list),
                             skip + 1)

-- Turn spare 256-byte hash into dense 16 by folding with xor
dense :: [Int] -> [Int]
dense nums = [ foldl xor 0 (take 16 $ drop (i*16) nums) | i <- [0..15] ]

-- Convert to ASCII values and add the 5 hard-coded numbers from the problem
lengths :: String -> [Int]
lengths s = map ord s ++ [17, 31, 73, 47, 23]

toHex :: Int -> String
toHex n = let hex = "0123456789abcdef"
          in  [hex !! (n `div` 16), 
               hex !! (n `mod` 16)]

main = do
  file <- readFile "10.input"
  let input = head $ lines file
  let lens = lengths input
  let (ns, _, _) = process [0..255] lens
  print $ concat $ map toHex $ dense ns
  
