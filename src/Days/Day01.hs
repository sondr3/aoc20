module Days.Day01 where

import qualified AoC
import Data.List (elemIndex)

main :: IO ()
main = do
  input <- AoC.getInput 1 AoC.number
  print (partA input)
  print (partB input)

partA :: [Int] -> Int
partA i = case elemIndex 2020 $ map sum g of
  Just idx -> product $ g !! idx
  Nothing -> 0
  where
    g = [[x, y] | x <- i, y <- i]

partB :: [Int] -> Int
partB i = case elemIndex 2020 $ map sum g of
  Just idx -> product $ g !! idx
  Nothing -> 0
  where
    g = [[x, y, z] | x <- i, y <- i, z <- i]
