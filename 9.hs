{- Advent of Code 2017

   Day: 9 - Stream Processing
   URL: http://adventofcode.com/2017/day/9

   Part 1/2
-}

import NanoParsec
import Control.Monad
import Control.Applicative

data Thing = Group [Thing] 
           | Garbage String 
             deriving (Show)

junk :: Parser String
junk = Parser $ 
  \s -> case s of
          ('!':c:cs) -> [("", cs)] -- Remove canceled chars
          ('>':cs)   -> []
          []         -> []
          (c:cs)     -> [([c],cs)]

garbage :: Parser Thing
garbage = do
  char '<';
  junks <- many junk
  char '>';
  return $ Garbage $ concat junks

comma :: Parser ([Thing] -> [Thing] -> [Thing])
comma = char ',' >> return (\a b -> a ++ b)

thing :: Parser [Thing]
thing = do
  s <- group <|> garbage
  return [s]

things :: Parser [Thing]
things = chainl thing comma ([])

group :: Parser Thing
group = do
  char '{';
  ts <- things
  char '}';
  return $ Group ts

count :: Int -> Thing -> Int
count n (Garbage _) = 0
count n (Group ts)  = n + sum (map (count (n+1)) ts)

measure :: Thing -> Int
measure (Garbage s) = length s
measure (Group ts) = sum $ map measure ts

main = do
  file <- readFile "9.input"
  let input = (lines file) !! 0
  let parsed = run group input
  print $ (count 1 parsed, measure parsed)
  
