{- Advent of Code 2017

   Day: 16 - Permutation Promenade
   URL: http://adventofcode.com/2017/day/16

   Part 1

   Remarks: This takes 5.6s to parse the 10,000 moves and 0.2s to perform
            them.  Part 2 of the challenge requires 5,000,000,000 iterations
            of these moves, which will take about 32 years to perform.
            So we'll need to rent a bunch of servers, or perhaps we can find 
            a shortcut...
-}

import Control.Applicative (some, (<|>))
import Data.List (elemIndex)
import Helpers (firstIndex)
import NanoParsec
import Stackoverflow (rotate, swap)

data Move = Spin Int
          | Exchange Int Int
          | Partner Char Char
            deriving (Show)

type Line = [Char]

slot :: Parser Int
slot = do
  digits <- some digit
  return $ read digits

spin :: Parser Move
spin = do
  char 's'
  i <- slot
  return $ Spin i

exchange :: Parser Move
exchange = do
  char 'x'
  i <- slot
  char '/'
  j <- slot
  return $ Exchange i j

partner :: Parser Move
partner = do
  char 'p'
  a <- oneOf ['a'..'p']
  char '/'
  b <- oneOf ['a'..'p']
  return $ Partner a b

move :: Parser [Move]
move = do
  m <- spin <|> exchange <|> partner
  return [m]

comma :: Parser ([a] -> [a] -> [a])
comma = char ',' >> return (\a b -> a ++ b)

moves :: Parser [Move]
moves = chainl move comma []

-- Perform one dance move on the line
perform :: Line -> Move -> Line
perform line move = 
  case move of
    Spin spin    -> rotate (length line - spin) line
    Exchange i j -> swap i j line
    Partner a b  -> let i = firstIndex a line
                        j = firstIndex b line
                    in  swap i j line

process :: Line -> [Move] -> Line
process = foldl perform

main = do
  file <- readFile "16.input"
  let input = lines file
  let parsed = run moves (input !! 0)
  print $ process ['a'..'p'] parsed

