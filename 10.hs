{- Advent of Code 2017

   Day: 10 - Knot Hash
   URL: http://adventofcode.com/2017/day/10

   Part 1
-}

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
process a = foldl f (a, 0, 0)
  where
    f (list, i, skip) len = (rev list i len,
                             (i + skip + len) `mod` length list,
                             skip + 1)

removecomma :: String -> String
removecomma = map (\c -> if c == ',' then ' ' else c)

main = do
  file <- readFile "10.input"
  let input = head $ lines file
  let lengths = map read $ words $ removecomma input
  print $ process [0..255] lengths
  
