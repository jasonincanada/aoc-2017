{- Advent of Code 2017

   Day: 5 - A Maze of Twisty Trampolines, All Alike
   URL: http://adventofcode.com/2017/day/5

   Part 1

   Remarks:  This runs out of stack space when compiled normally.
             Compile with -rtsopts, then call with ./5-1 +RTS -K128M
-}

type Cursor = Int
type List = ([Int], Cursor)

-- Increment the nth number in the list
incr :: [Int] -> Cursor -> [Int]
incr xs n = let left = take (n-1) xs
                right = drop n xs
            in  left ++ [1 + (xs !! (n-1))] ++ right

next :: List -> List
next (nums, index) = let jump = nums !! (index-1)
                     in  (incr nums index, index + jump)

countSteps :: List -> Int
countSteps list = go list 0
  where
    go (list, c) n
      | c > length list = n 
      | otherwise       = let nextn = n + 1
                              nextlist = next (list, c)
                          in  go nextlist nextn

main = do
  file <- readFile "5.input"
  print $ countSteps $ (map read $ lines file, 1)

