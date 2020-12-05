module Days.Day05 where

import qualified AoC
import Data.Bifunctor (Bifunctor (bimap))
import Data.List ((\\))

data Dir = Lower | Upper deriving (Show, Eq)

dirFromString :: [Char] -> [Dir]
dirFromString [] = []
dirFromString ('F' : xs) = Lower : dirFromString xs
dirFromString ('L' : xs) = Lower : dirFromString xs
dirFromString ('B' : xs) = Upper : dirFromString xs
dirFromString ('R' : xs) = Upper : dirFromString xs
dirFromString _ = undefined

dirs :: [Char] -> ([Dir], [Dir])
dirs s = (dirFromString (take 7 s), dirFromString (drop 7 s))

partition :: [Int] -> [Dir] -> Int
partition _ [] = undefined
partition [_, y] [Upper] = y
partition [x, _] [Lower] = x
partition xs (Upper : ys) = partition (snd $ halve xs) ys
partition xs (Lower : ys) = partition (fst $ halve xs) ys

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

ids :: [[Char]] -> [Int]
ids = map (((\x -> (fst x * 8) + snd x) . bimap (partition [0 .. 127]) (partition [0 .. 7])) . dirs)

partA :: [String] -> Int
partA input = maximum $ ids input

partB :: [String] -> Int
partB input = head ([minimum xs .. maximum xs] \\ xs)
  where
    xs = ids input

main :: IO ()
main = do
  input <- lines <$> readFile (AoC.inputName 5)
  print (maximum $ ids input)
  print (partB input)
