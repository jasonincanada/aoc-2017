{- Advent of Code 2017

   Day: 3 - Spiral Memory
   URL: http://adventofcode.com/2017/day/3

   Part 1
-}

-- Highest number on ring i
ringMax :: Int -> Int
ringMax i = (2*i+1)^2

-- Index of the ring that n is found on
findRing :: Int -> Int
findRing n = let tuples = map (\x -> (x, ringMax x)) [0..]
                 list = filter (\(i, max) -> max >= n) tuples
             in  fst $ head list

-- Compute number of steps to the middle. This is the index of the ring
-- plus the distance from the target number to the nearest cardinal
steps :: Int -> Int
steps n = let i = findRing n
              max = ringMax i
              size = ringMax i - ringMax (i-1)
              row = size `div` 4
              halfrow = row `div` 2
              cardinals = [ max - (i*row) + halfrow | i <- [1..4] ]
              distances = [ abs (n-c) | c <- cardinals ]
              closest = minimum distances
          in  i + closest

main = print $ steps 368078

