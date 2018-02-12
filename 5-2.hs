{- Advent of Code 2017

   Day: 5 - A Maze of Twisty Trampolines, All Alike
   URL: http://adventofcode.com/2017/day/5

   Part 2

   Remarks:  This runs out of memory when compiled normally.
             Compile with -rtsopts, then call with ./5-1 +RTS -K128M

             Using Data.Sequence instead of the built-in List reduces run
             time from never (I canceled it after 23 minutes of waiting) 
             to 26 sec
-}

import Data.Sequence as DS

type Cursor = Int
type List = (DS.Seq Int, Cursor)

-- Increment the nth number in the list
incr :: DS.Seq Int -> Cursor -> Int -> DS.Seq Int
incr xs n a = let num = DS.index xs n
                  next = num + a
              in  DS.update n next xs

next :: DS.Seq Int -> Int -> List
next nums index = let jump = DS.index nums index
                  in  (incr nums index (if jump >= 3 then -1 else 1), index + jump)

len :: DS.Seq Int -> Int
len = DS.length

countSteps :: DS.Seq Int -> Int
countSteps list = go (list, 0) 0
  where
    go (list, i) n
      | i >= len list = n
      | otherwise     = let nextlist = next list i
                        in  go nextlist (n+1)

main = do
  file <- readFile "5.input"
  print $ countSteps $ DS.fromList (map read $ lines file)

