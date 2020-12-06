module Main where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

type Slope = (Int, Int)

main :: IO ()
main = do
  input <- BS.readFile "input.txt"
  print $ solve1 input
  print $ solve2 input

count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred

isTree :: Char -> Bool
isTree = (==) '#'

countTrees :: [Char] -> Int
countTrees = count isTree

solve1 :: ByteString -> Int
solve1 input = solveSlope input (3, 1)

solve2 :: ByteString -> Int
solve2 input = product $ solveSlope input <$> slopes
  where slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

solveSlope :: ByteString -> Slope -> Int
solveSlope input (right, down) = countTrees $ take m $ map (BS.index input) it
  where n = (+1) $ fromJust $ BS.elemIndex '\n' input
        m = ceiling $ fromIntegral (BS.length input) / fromIntegral (n * down)
        next (x, y) = ((x + right) `mod` (n - 1), y + down)
        toIndex (x, y) = x + y * n
        it = map toIndex $ iterate next (0, 0)
