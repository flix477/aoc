module Main where

import Lib

main :: IO ()
main = do
  input <- readFile "input.txt"
  let prg = parseInput input
  let machine = state prg
  let p = (fst patchRange, fst patchRange)
  let patch = findPatchForOutput machine 19690720 p
  putStrLn $ show $ patch
