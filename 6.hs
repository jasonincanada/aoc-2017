{- Advent of Code 2017

   Day: 6 - Memory Reallocation
   URL: http://adventofcode.com/2017/day/6

   Part 1/2
   
   Remarks:  We treat the list as a vector and in each cycle of the algo
             construct a "spread" vector, which is the max element
             spread over v_i as described in the problem, and a "subtraction"
             vector that removes the max element.  Then list n+1 is list n
             with these two vectors added to it with usual element-wise
             vector addition.

             Parts 1 and 2 are computed together and returned in a 2-tuple

Scratch notes showing the example in the problem worked out as
individual vectors: 

input   [0, 2, 7, 0]
      + [2, 2, 1, 2]   -> [2, 2, 2, 1] rot i
      - [0, 0, 7, 0]
      = [2, 4, 1, 2] ------------------------

        [2, 4, 1, 2]
      + [1, 1, 1, 1]
      - [0, 4, 0, 0]
      = [3, 1, 2, 3]

        [3, 1, 2, 3]
      + [0, 1, 1, 1]
      - [3, 0, 0, 0]
      = [0, 2, 3, 4]

        [0, 2, 3, 4]
      + [1, 1, 1, 1]
      - [0, 0, 0, 4]
      = [1, 3, 4, 1]

        [1, 3, 4, 1]
      + [1, 1, 1, 1]
      - [0, 0, 4, 0]
      = [2, 4, 1, 2] ------------------------
-}

import Data.List

input = [4, 10, 4, 1, 8, 4, 9, 14, 5, 1, 14, 15, 0, 15, 3, 5]

-- From https://stackoverflow.com/questions/16378773/rotate-a-list-in-haskell
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

-- Create the "spread" vector which spreads the value n over d elements
-- spreadvec 10 7 = [2,2,2,1,1,1,1]
spreadvec :: Int -> Int -> [Int]
spreadvec n d = let q = n `div` d
                    r = n `mod` d
                in  replicate r (q+1) ++ replicate (d-r) q

-- subvec 3 4 = [-3, 0, 0, 0]
subvec :: Int -> Int -> [Int]
subvec m l = -m : replicate (l-1) 0

firstIndex :: Eq a => a -> [a] -> Int
firstIndex n xs = case elemIndex n xs of
                    Nothing -> 0
                    (Just x) -> x

next :: [Int] -> [Int]
next list = let l = length list
                m = maximum list
                i = firstIndex m list
                spr = rotate (l-i-1) $ spreadvec m l
                sub = rotate (l-i) $ subvec m l
            in  zipWith (+) list (zipWith (+) spr sub)
                

process :: [Int] -> (Int, Int)
process list = go list 1 [list]
  where
    go list n seen = let nextlist = next list
                     in  if nextlist `elem` seen
                         then (n, n - firstIndex nextlist (reverse seen) + 1)
                         else go nextlist (n+1) (list : seen)

main = print $ process input

