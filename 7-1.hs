{- Advent of Code 2017

   Day: 7 - Recursive Circus
   URL: http://adventofcode.com/2017/day/7

   Part 1
-}

import Data.Char (isDigit)

data Program = Program { name :: String,
                         weight :: Int,
                         children :: [String] } deriving (Show)

-- Parse a line into our internal Program type
parse :: String -> Program
parse s = let ws = words s
              name = head ws
              weight = read $ filter isDigit $ ws !! 1
              children = map (filter (/=',')) $ drop 3 ws
          in  Program { name = name, 
                        weight = weight, 
                        children = children }

-- Given a program, find its parent in the passed list of all programs
parent :: Program -> [Program] -> Maybe Program
parent p [] = Nothing
parent p (c:cs)
  | name p `elem` children c = Just c
  | otherwise                = parent p cs

process :: [Program] -> String
process [] = "No input"
process (p:ps) = go p ps
  where
    go :: Program -> [Program] -> String
    go p ps = case parent p ps of
                Nothing -> name p
                Just p  -> go p ps

main = do
  file <- readFile "7.input"
  let input = lines file
  let programs = map parse input
  print $ process programs
  
