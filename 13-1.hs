{- Advent of Code 2017

   Day: 13 - Packet Scanners
   URL: http://adventofcode.com/2017/day/13

   Part 1
-}

type Depth = Int
type Range = Int

-- positions 4 = [1,2,3,4,3,2,1,2,3,4,3,...]
positions :: Range -> [Int]
positions r = cycle $ [1..r] ++ [r-1,r-2..2]

process :: [(Depth, Range)] -> [Int]
process drs = map f drs
  where
    f (d, r) = let x = (range r) !! d
               in  if x == 1 
                   then d * r
                   else 0

removecolon :: String -> String
removecolon = map (\c -> if c == ':' then ' ' else c)

tuple :: [a] -> (a, a)
tuple [d, r] = (d, r)

main = do
  file <- readFile "13.input"
  let input = lines file
  let drs = map (tuple . map read . words . removecolon) input
  print $ sum $ process drs

