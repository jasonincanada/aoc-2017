{- Advent of Code 2017

   Day: 8 - I Heard You Like Registers
   URL: http://adventofcode.com/2017/day/8

   Part 1/2 - Outputs a tuple with answers to both parts together
-}

-- A register is its name paired with the history of its values
type Register = (String, [Int])
type CPU = [Register]
type Line = (String, Int, String, (Int -> Bool))

add :: CPU -> String -> Int -> CPU
add [] name val = [(name, [val])]
add (r@(n,vs):rs) name val 
  | n == name = addVal r val : rs
  | otherwise = r : add rs name val
  where
    addVal (n, vs) val = (n, (head vs) + val : vs)

test :: CPU -> String -> (Int -> Bool) -> Bool
test []          name f = f 0
test ((n,vs):rs) name f
  | n == name = f $ head vs
  | otherwise = test rs name f

predicate :: String -> Int -> (Int -> Bool)
predicate s v
  | s == "==" = (==) v
  | s == "!=" = (/=) v
  | s == "<"  = flip (<) v
  | s == ">"  = flip (>) v
  | s == ">=" = flip (>=) v
  | s == "<=" = flip (<=) v

parse :: String -> Line
parse s = let ws = words s
              reg = ws !! 0
              mult = if ws !! 1 == "inc" then 1 else -1
              val = read $ ws !! 2
              testreg = ws !! 4
              pred = predicate (ws !! 5) (read $ ws !! 6)
          in  (reg, mult*val, testreg, pred)

-- The starting CPU is empty, we learn the registers as they're set
cpu :: CPU
cpu = []

doOp :: Line -> CPU -> CPU
doOp (reg, val, testreg, pred) cpu
  | test cpu testreg pred = add cpu reg val
  | otherwise             = cpu

process :: [String] -> (Int, Int)
process lines = let commands = map parse $ reverse lines
                    final = foldr doOp cpu commands
                    maxnow = maximum $ map (head . snd) final
                    maxever = maximum $ concat $ map snd final
                in  (maxnow, maxever)

main = do
  file <- readFile "8.input"
  let input = lines file
  print $ process $ input
  
