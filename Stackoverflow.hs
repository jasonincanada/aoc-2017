-- Code I've found on stackoverflow.com

module Stackoverflow where

-- https://stackoverflow.com/questions/16378773/rotate-a-list-in-haskell
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

-- https://stackoverflow.com/questions/30551033/swap-two-elements-in-a-list-by-its-indices
-- Modified by me to account for j > i
swap :: Int -> Int -> [a] -> [a]
swap i' j' xs = let (i, j) = if i' < j' then (i', j') else (j', i')
                    elemI = xs !! i
                    elemJ = xs !! j
                    left = take i xs
                    middle = take (j - i - 1) (drop (i + 1) xs)
                    right = drop (j + 1) xs
                in  left ++ [elemJ] ++ middle ++ [elemI] ++ right

