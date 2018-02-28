module Helpers where

import Data.List (elemIndex)

firstIndex :: Eq a => a -> [a] -> Int
firstIndex n xs = case elemIndex n xs of
                    Nothing -> 0
                    (Just x) -> x

