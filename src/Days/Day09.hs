module Days.Day09 where

import qualified AoC
import Data.Bifunctor (Bifunctor (first))
import Data.List (tails)

windows :: Int -> [a] -> [[a]]
windows m = foldr (zipWith (:)) (repeat []) . take m . tails

validNum :: (Eq a, Num a) => a -> [a] -> Bool
validNum n xs = or [True | x <- xs, y <- xs, x + y == n]

partA :: [Int] -> Int -> Int
partA nums n = fst . head . filter (not . snd) $ zip (map fst xs) (map (uncurry validNum) xs)
  where
    xs = zipWith (curry (first head)) (windows 1 (drop n nums)) (windows n nums)

partB :: [Int] -> Int -> Int
partB nums n = sum [minimum ns, maximum ns]
  where
    ns = concat $ filter (not . null) $ concatMap s xs
    xs = zip (repeat n) (map (`windows` nums) [2 .. length nums])
    s (y, ys) = map fst $ filter snd $ map (\x -> (x, sum x == y)) ys

main :: IO ()
main = do
  input <- map read . lines <$> readFile (AoC.inputName 9)
  let invalid = partA input 25
  print invalid
  print (partB input invalid)
