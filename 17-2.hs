{- Advent of Code 2017

   Day: 17 - Spinlock
   URL: http://adventofcode.com/2017/day/17

   Part 2
-}

next :: Int -> Int -> Int
next shift n = go n 1
  where
    go n i   = let n' = n + 1
                   i' = (i + shift) `mod` n'
               in  if i' == 0
                   then n'
                   else go n' (i' + 1)

-- Distinct numbers after the 0:                                                                      
-- [1,2,4,7,16,33,146,483,9754,17280,27434,28427,148956,152529,716247,937911,2143773,4771058,11918888,31220910,...
main = do
  let input = 337
  let until = 50000000
  print $ takeWhile (< until) $ iterate (next input) 1

