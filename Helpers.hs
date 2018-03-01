module Helpers where

import Data.List (elemIndex)

firstIndex :: Eq a => a -> [a] -> Int
firstIndex n xs = case elemIndex n xs of
                    Nothing -> 0
                    (Just x) -> x

modifyNth :: Int -> (a -> a) -> [a] -> [a]
modifyNth _ _ [] = []
modifyNth n f (x:xs)
  | n == 0    = f x : xs
  | otherwise = x : modifyNth (n-1) f xs
