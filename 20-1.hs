{- Advent of Code 2017

   Day: 20 - Particle Swarm
   URL: http://adventofcode.com/2017/day/20

   Part 1
-}

import Data.List (sortBy)
import Data.Ord (comparing)

type Vec3 = (Int, Int, Int)

data Particle = Particle { pos :: Vec3,
                           vel :: Vec3,
                           acc :: Vec3 
                         } deriving (Show)

-- Manhattan distance to the origin
dist :: Vec3 -> Int
dist (x, y, z) = abs x + abs y + abs z

closest :: [(Int, Particle)] -> (Int, Particle)
closest = head . sorted
  where
    sorted = sortBy comp
    comp   = comparing $ dist . acc . snd

-- Parse a particle from an input line
parse :: String -> Particle
parse s = let nums = map read . words $ map digits s
          in  Particle { pos = (nums !! 0, nums !! 1, nums !! 2),
                         vel = (nums !! 3, nums !! 4, nums !! 5),
                         acc = (nums !! 6, nums !! 7, nums !! 8) }
  where digits s = if s `elem` ('-' : ['0'..'9'])
                   then s
                   else ' '

main = do
  file <- readFile "20.input"
  let particles = zipWith (,) [0..] $ map parse $ lines file
  print $ closest particles

