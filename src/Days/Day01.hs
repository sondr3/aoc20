module Days.Day01 where

import qualified AoC

main :: IO ()
main = do
  input <- AoC.getInput 1 AoC.number
  print (partA input)
  print (partB input)

partA :: [Int] -> Int
partA i = head [x * y | x <- i, y <- i, x + y == 2020]

partB :: [Int] -> Int
partB i = head [x * y * z | x <- i, y <- i, z <- i, x + y + z == 2020]
