{- Advent of Code 2017

   Day: 14 - Disk Defragmentation
   URL: http://adventofcode.com/2017/day/14

   Part 0 - Generate the list of rows that make up the grid.
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

compute :: [a] -> [Int] -> [a]
compute as lengths = 
  let (xs, _, _) = foldl f (as, 0, 0) (concat $ replicate 64 lengths)
  in   xs
    where
      f (list, i, skip) len = (rev list i len,
                               (i + skip + len) `mod` (length list),
                               skip + 1)

-- Turn sparse 256-byte hash into dense 16
dense :: [Int] -> [Int]
dense nums = [ foldl xor 0 (take 16 $ drop (i*16) nums) | i <- [0..15] ]

toHex :: Int -> String
toHex n = let hex = "0123456789abcdef"
          in  [hex !! (n `div` 16), 
               hex !! (n `mod` 16)]

toGrid :: Char -> String
toGrid '0' = "...."
toGrid '1' = "...#"
toGrid '2' = "..#."
toGrid '3' = "..##"
toGrid '4' = ".#.."
toGrid '5' = ".#.#"
toGrid '6' = ".##."
toGrid '7' = ".###"
toGrid '8' = "#..."
toGrid '9' = "#..#"
toGrid 'a' = "#.#."
toGrid 'b' = "#.##"
toGrid 'c' = "##.."
toGrid 'd' = "##.#"
toGrid 'e' = "###."
toGrid 'f' = "####"

hash :: String -> String
hash s = let sparse = compute [0..255] (map ord s ++ [17, 31, 73, 47, 23])
             hash = dense sparse
             grid = concat $ map toGrid $ concat $ map toHex hash
         in  grid

process :: String -> [String]
process s = let numbered = map (\n -> s ++ "-" ++ show n) [0..127]
            in  map hash numbered

main = do
  let input = "jxqlasbh"
  print $ process input

