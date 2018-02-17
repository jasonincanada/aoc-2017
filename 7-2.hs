{- Advent of Code 2017

   Day: 7 - Recursive Circus
   URL: http://adventofcode.com/2017/day/7

   Part 2

   Remarks: Ugly code, needs improvement
-}

import Data.Char (isDigit)
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)
import Data.Function (on)

data Program = Program { name :: String,
                         weight :: Int,
                         children :: [String] } deriving (Show)

data Tree a = Tree a [Tree a] deriving (Show)

-- Parse a line into our internal Program type
parse :: String -> Program
parse s = let ws = words s
              name = ws !! 0
              weight = read $ filter (isDigit) $ ws !! 1
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

findParent :: [Program] -> Program
findParent (p:ps) = go p ps
  where
    go :: Program -> [Program] -> Program
    go p ps = case parent p ps of
                Nothing -> p
                Just p  -> go p ps

programByName :: [Program] -> String -> Program
programByName ps n = head $ filter (\p -> name p == n) ps

-- Given a program's name and the list of programs, build a tree
build :: [Program] -> String -> Tree (String, Int)
build ps n = let p = programByName ps n 
                 nodes = map (build ps) $ children p 
             in  Tree (name p, weight p) nodes

totalweight :: Tree (String, Int) -> Int
totalweight (Tree (name, weight) children) = weight + sum (map totalweight children)

-- Add a third value to the tuple which is the total weight of the node 
-- after adding its children's weights
weigh :: Tree (String, Int) -> Tree (String, Int, Int)
weigh tree@(Tree (name, weight) children) =
  Tree (name, weight, totalweight tree) (map weigh children)

tn (Tree (n, _, _) _) = n
w  (Tree (_, w, _) _) = w
tw (Tree (_, _, t) _) = t
tc (Tree (_, _, _) c) = c

oddOneOut :: (Ord b, Eq b) => [a] -> (a -> b) -> Maybe a
oddOneOut cs f = let grouped = groupBy ((==) `on` f) . sortBy (comparing f)
                     singletons = filter ((==1) . length) (grouped cs)
                 in  if length singletons == 0
                     then Nothing
                     else Just $ head (head singletons)

process :: Tree (String, Int, Int) -> [Int]
process (Tree (name, weight, totalweight) children) =
  case oddOneOut children tw of
    Just tree -> let cs = tc tree
                 in  case oddOneOut cs tw of
                       Just _  -> process tree
                       Nothing -> let weights = map tw children
                                      min = minimum weights
                                      max = maximum weights
                                  in  [w tree, min, max]

main = do
  file <- readFile "7.input"
  let input = lines file
      programs = map parse input
      parent = findParent programs
      tree = build programs $ name parent
      weighed = weigh tree
  print $ process weighed
  
