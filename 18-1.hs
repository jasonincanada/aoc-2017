{- Advent of Code 2017

   Day: 18 - Duet
   URL: http://adventofcode.com/2017/day/18

   Part 1
-}

import Data.Char (ord)
import NanoParsec (char, number, oneOf, Parser, run, space, string, run)
import Control.Applicative ((<|>))
import Helpers (modifyNth)

-- Describe the CPU and its opcodes
data CPU = CPU { registers :: [Int],
                 freq      ::  Int,
                 ip        ::  Int,
                 halted    ::  Bool
               } deriving (Show)

type Register = Char

data Value = Reg Register 
           | Literal Int
           deriving (Show)

data Op = Add     Register Value
        | Jump    Value    Value
        | Mod     Register Value
        | Mult    Register Value
        | Recover Register
        | Set     Register Value
        | Sound   Value
        deriving (Show)

start :: CPU
start = CPU { registers = [ 0 | r <- ['a'..'z'] ],
              freq      = 0,
              ip        = 0,
              halted    = False }

-- Parsing of commands
register :: Parser Value
register = do
  reg <- oneOf ['a'..'z']
  return $ Reg reg

literal :: Parser Value
literal = do
  lit <- number
  return $ Literal lit

value :: Parser Value
value = register <|> literal

-- Generic parser for a "Cmd Register Value"-formatted instruction
parse_crv :: String -> (Register -> Value -> Op) -> Parser Op
parse_crv cmd ctor = do
  string cmd
  space
  reg <- register
  space
  val <- value
  return $ let (Reg r) = reg 
           in  ctor r val

add, modu, mult, set :: Parser Op
add  = parse_crv "add" Add
modu = parse_crv "mod" Mod
mult = parse_crv "mul" Mult
set  = parse_crv "set" Set

jump :: Parser Op
jump = do
  string "jgz "
  a <- value
  space
  b <- value
  return $ Jump a b

recover :: Parser Op
recover = do
  string "rcv "
  reg <- register
  return $ let (Reg r) = reg
           in  Recover r

sound :: Parser Op
sound = do
  string "snd "
  v <- value
  return $ Sound v

opcode :: Parser Op
opcode =     add 
         <|> jump 
         <|> modu
         <|> mult
         <|> recover
         <|> set
         <|> sound

-- CPU implementation
getValue :: CPU -> Value -> Int
getValue cpu val = case val of
  Literal v -> v
  Reg r     -> (registers cpu) !! (ord r - ord 'a')

setValue :: CPU -> Register -> Value -> CPU
setValue cpu r v = let val = getValue cpu v
                       i   = ord r - ord 'a'
                       rs  = registers cpu
                       rs' = modifyNth i (const val) rs
                   in  cpu { registers = rs' }

-- Advance the instruction pointer
advance :: CPU -> Int -> CPU
advance cpu n = cpu { ip = (ip cpu) + n }

-- Common code factored out of Add, Mod, Mult
op_crv :: CPU -> (Int -> Int -> Int) -> Register -> Value -> CPU
op_crv cpu f reg val = 
  let v'  = f (getValue cpu (Reg reg)) (getValue cpu val)
  in  setValue cpu reg (Literal v')

instruct :: CPU -> Op -> CPU
instruct cpu op =
  case op of

    -- Basic register operations
    Add reg val  -> advance (op_crv cpu (+) reg val) 1
    Mod reg val  -> advance (op_crv cpu mod reg val) 1
    Mult reg val -> advance (op_crv cpu (*) reg val) 1

    -- Play a sound
    Sound val    -> advance (cpu { freq = getValue cpu val }) 1
     
    -- Set a register
    Set reg val  -> advance (setValue cpu reg $ Literal (getValue cpu val)) 1

    -- Recover the last frequency if the register is not 0
    -- Part 1 requires us to halt if the register being set was not 0
    Recover reg  -> if (getValue cpu (Reg reg)) /= 0
                    then halt $ setValue cpu reg (Literal $ freq cpu)
                    else advance cpu 1

    -- Jump if greater than zero
    Jump v offs  -> if (getValue cpu v) /= 0
                    then advance cpu $ getValue cpu offs
                    else advance cpu 1

halt :: CPU -> CPU
halt cpu = cpu { halted = True }

-- Run a list of commands on the CPU
runcommands :: CPU -> [Op] -> CPU
runcommands cpu ops 
  | halted cpu           = cpu
  | ip cpu < 0           = halt cpu
  | ip cpu >= length ops = halt cpu
  | otherwise            = let op = ops !! (ip cpu)
                               cpu' = instruct cpu op
                           in  runcommands cpu' ops

main = do
  file <- readFile "18.input"
  let input = lines file
  let parsed = map (run opcode) input 
  print $ runcommands start parsed

-- Output:
-- CPU {registers = [2951,2951,0,0,0,0,0,0,126,0,0,0,0,0,0,1842102951,0,0,0,0,0,0,0,0,0,0], freq = 2951, ip = 25, halted = True}

