{- Advent of Code 2017

   Day: 17 - Spinlock
   URL: http://adventofcode.com/2017/day/17

   Part 1
-}

import Helpers (firstIndex)

type Shift = Int
type Lock = [Int]

process :: Shift -> (Lock, Int)
process shift = go 2017 [0] 0
  where
    go 0 lock i = (lock, i)
    go n lock i = let len   = length lock
                      max   = len - 1
                      next  = max + 1
                      i'    = (i + shift) `mod` len + 1
                      lock' = take i' lock ++ [next] ++ drop i' lock
                  in  go (n-1) lock' i'

main = do
  let input = 337
  let (lock, i) = process input
  print $ lock !! (i+1)

