{- Advent of Code 2017

   Day: 13 - Packet Scanners
   URL: http://adventofcode.com/2017/day/13

   Part 2
-}

type Depth = Int
type Range = Int
type Scanner = (Depth, Range)

hit :: Range -> Int -> Bool
hit range t = let cycle = 2 * (range - 1)
                  position = t `mod` cycle
              in  position == 0

-- Return the lowest wait time (possibly zero) required to get all the way through the firewall
process :: [Scanner] -> Int
process scanners = go 0
  where
    go wait = let attempt = [ hit range (depth + wait) | (depth, range) <- scanners ]
                  caught  = or attempt
              in  if caught
                  then go (1 + wait)
                  else wait

removecolon :: String -> String
removecolon = map (\c -> if c == ':' then ' ' else c)

tuple :: [a] -> (a, a)
tuple [d, r] = (d, r)

main = do
  file <- readFile "13.input"
  let input = lines file
  let scanners = map (tuple . map read . words . removecolon) input
  print $ process scanners

