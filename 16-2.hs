{- Advent of Code 2017

   Day: 16 - Permutation Promenade
   URL: http://adventofcode.com/2017/day/16

   Part 2
-}

import Control.Applicative (some, (<|>))
import Data.List (elemIndex, isPrefixOf)
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
comma = char ',' >> return (++)

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

process :: Line -> [Move] -> Int -> [Line]
process line moves = go line
  where
    go _    0     = []
    go line count = let next = foldl perform line moves
                    in  next : go next (count-1)

main = do
  file <- readFile "16.input"
  let input = lines file
  let parsed = run moves (head input)
  print $ process ['a'..'p'] parsed (63+1)

