module Days.Day03 where

import qualified AoC

type Pos = (Int, Int)

type Size = (Int, Int)

data World = World
  { zWorld :: [[Char]],
    zSize :: Size
  }
  deriving (Show)

move :: Pos -> World -> Int -> Int -> Pos
move (x, y) (World _ (w, _)) x' y' = ((x + x') `mod` w, y + y')

buildWorld :: [String] -> World
buildWorld s = do
  let width = length $ head s
      heigth = length s
   in World {zWorld = s, zSize = (width, heigth)}

walk :: World -> Int -> Int -> [Pos]
walk w@(World _ (_, h)) x y = takeWhile (\z -> snd z < h) (step (0, 0) x y)
  where
    step p x' y' = p : step (move p w x' y') x' y'

isTree :: Pos -> World -> Bool
isTree (x, y) (World w _) = col == '#'
  where
    col = row !! x
    row = w !! y

trees :: World -> Int -> Int -> Int
trees w x y = length $ filter (== True) $ map (`isTree` w) $ walk w x y

partA :: World -> Int
partA w = trees w 3 1

partB :: World -> Int
partB w = product [trees w 1 1, trees w 3 1, trees w 5 1, trees w 7 1, trees w 1 2]

main :: IO ()
main = do
  input <- lines <$> readFile (AoC.inputName 3)
  let world = buildWorld input
  print (partA world)
  print (partB world)
