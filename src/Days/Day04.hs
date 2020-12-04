{-# LANGUAGE OverloadedStrings #-}

module Days.Day04 where

import qualified AoC
import Data.Char (isDigit, isHexDigit)
import Data.Text (Text)
import qualified Data.Text as T

toTuple :: [Text] -> (Text, Text)
toTuple [x, y] = (x, y)
toTuple _ = undefined

blocks :: Text -> [[(Text, Text)]]
blocks input = map (map (toTuple . T.splitOn ":") . T.splitOn " " . T.unwords . T.lines) (T.splitOn "\n\n" input)

hasCid :: [(Text, Text)] -> Bool
hasCid xs = case lookup "cid" xs of
  Just _ -> True
  Nothing -> False

between :: Int -> Int -> Int -> Bool
between x mi ma = x >= mi && x <= ma

validHeight :: (Text, Text) -> Bool
validHeight (height, "cm") = between (read $ T.unpack height) 150 193
validHeight (height, "in") = between (read $ T.unpack height) 59 76
validHeight _ = False

validB :: [(Text, Text)] -> [Bool]
validB = map valid
  where
    valid :: (Text, Text) -> Bool
    valid ("byr", val) = T.length val == 4 && between (read $ T.unpack val) 1920 2002
    valid ("iyr", val) = T.length val == 4 && between (read $ T.unpack val) 2010 2020
    valid ("eyr", val) = T.length val == 4 && between (read $ T.unpack val) 2020 2030
    valid ("hgt", val) = validHeight $ T.splitAt (T.length val - 2) val
    valid ("hcl", val) = T.head val == '#' && T.length (T.tail val) == 6 && all isHexDigit (T.unpack $ T.tail val)
    valid ("ecl", val) = val `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    valid ("pid", val) = T.length val == 9 && all isDigit (T.unpack val)
    valid ("cid", _) = True
    valid _ = False

validA :: [[(Text, Text)]] -> [[(Text, Text)]]
validA = filter (\x -> length x == 8 || (length x == 7 && not (hasCid x)))

partA :: [[(Text, Text)]] -> Int
partA input = length $ validA input

partB :: [[(Text, Text)]] -> Int
partB input = length $ filter (all (== True)) $ map validB (validA input)

main :: IO ()
main = do
  input <- blocks . T.pack <$> readFile (AoC.inputName 4)
  print (partA input)
  print (partB input)
