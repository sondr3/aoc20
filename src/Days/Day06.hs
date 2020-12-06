module Days.Day06 where

import qualified AoC
import Data.List (intersect, nub)
import Data.List.Split (splitOn)

partA :: [Char] -> Int
partA input = sum $ map (length . nub . filter (`elem` ['a' .. 'z'])) (splitOn "\n\n" input)

partB :: [Char] -> Int
partB input = sum . map (length . foldl1 intersect . lines) $ splitOn "\n\n" input

main :: IO ()
main = do
  input <- readFile (AoC.inputName 6)
  print (partA input)
  print (partB input)
