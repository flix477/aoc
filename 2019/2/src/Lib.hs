-- https://adventofcode.com/2019/day/2
-- not very happy with the quality of this one, might revisit

module Lib (
  parseInput,
  state,
  patchRange,
  findPatchForOutput,
) where

import Data.List.Split (splitOn)

data State = State {
  pc :: Int,
  program :: [Int],
  completed :: Bool
}

state :: [Int] -> State
state prg = State 0 prg False

data Operation = Add | Mult | End

-- |Parses opcodes from an input string
parseInput :: [Char] -> [Int]
parseInput = map (\x -> read x :: Int) . splitOn ","

patchRange = (0, 99)

-- |Finds the patch needed for a program to return a specific value
findPatchForOutput :: State -> Int -> (Int, Int) -> (Int, Int)
findPatchForOutput s o p =
  if executeWithParams s p == o
  then p
  else findPatchForOutput s o $ nextPatch patchRange p

-- |Gets the next possible patch value
nextPatch :: (Int, Int) -> (Int, Int) -> (Int, Int)
nextPatch (min, max) (a, b)
  | b < max = (a, b + 1)
  | b == max && a < max = (a + 1, min)
  | otherwise = (min, min)

-- "Each time you try a pair of inputs, make sure you first reset
-- the computer's memory to the values in the program (your puzzle input)
-- in other words, don't reuse memory from a previous attempt."
-- bro do you even immutable

-- |Patches a program and executes it to get its output
executeWithParams :: State -> (Int, Int) -> Int
executeWithParams s a =
  let prg = program s
      newPrg = patchProgram prg a
      newS = s { program = newPrg }
  in getOutput $ executeToEnd newS

-- |Patches a program to change the second and third values
patchProgram :: [Int] -> (Int, Int) -> [Int]
patchProgram (x:xs) (a, b) = [x, a, b] ++ drop 2 xs

-- |Returns the output of a program after executing it
getOutput :: State -> Int
getOutput s =
  let prg = program s
  in prg !! 0

-- |Performs CPU cycles on the state until a halt operation is found
executeToEnd :: State -> State
executeToEnd s@State{completed=True} = s
executeToEnd s = executeToEnd $ cpuCycle s

-- |Performs a CPU cycle on the state, fetching and
-- performing an operation if needed
cpuCycle :: State -> State
cpuCycle s =
  let opcode = fetchAtPC s
      operation = parseOpcode opcode
  in case operation of
    Just op -> performOperation s op
    Nothing -> s { pc = pc s + 1 }

-- |Performs an operation on the state
performOperation :: State -> Operation -> State
performOperation s End = s { completed = True }
performOperation s Add = performArithmeticOp s (+)
performOperation s Mult = performArithmeticOp s (*)

-- |Performs an arithmetic operation on the state
performArithmeticOp :: State -> (Int -> Int -> Int) -> State
performArithmeticOp s x =
  let (lhs, rhs, outPtr) = getArithmeticOpInfo s
      result = x lhs rhs
      prg = program s
      newPrg = replace prg (outPtr, result)
      p = pc s
  in s { program = newPrg, pc = p + 4 }

-- |Fetches the left and right hand sides of an arithmetic operation
-- as well as the output address
getArithmeticOpInfo :: State -> (Int, Int, Int)
getArithmeticOpInfo s =
  let fetch = fetchFromPC s
      lhs = fetchAt s $ fetch 1
      rhs = fetchAt s $ fetch 2
      outPtr = fetch 3
  in (lhs, rhs, outPtr)

-- |Fetches the value in the state's program at a specified distance
-- from the program counter
fetchFromPC :: State -> Int -> Int
fetchFromPC s i =
  let p = pc s
  in fetchAt s (p + i)

-- |Fetches the value in the state's program at the program counter
fetchAtPC :: State -> Int
fetchAtPC s = fetchAt s $ pc s

-- |Fetches the value in the state's program at a specified index
fetchAt :: State -> Int -> Int
fetchAt s i =
  let l = program s
  in l !! i

-- |Parses a given program opcode into an operation
parseOpcode :: Int -> Maybe Operation
parseOpcode 1 = Just Add
parseOpcode 2 = Just Mult
parseOpcode 99 = Just End
parseOpcode _ = Nothing

-- |Replaces the element in a list at a specified index
replace :: [a] -> (Int, a) -> [a]
replace [] _ = []
replace (_:xs) (0, a) = a:xs
replace (x:xs) (n, a) =
  if n < 0
  then (x:xs)
  else (x:replace xs (n - 1, a))
