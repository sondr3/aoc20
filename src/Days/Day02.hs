module Days.Day02 where

import qualified AoC
import AoC.Parse (Parser)
import Control.Monad (void)
import Data.Char (isLetter)
import Data.Functor (($>))
import Data.List (group, sort)
import Text.Megaparsec
import Text.Megaparsec.Char

type Line = (Int, Int, Char, String)

pLine :: Parser Line
pLine = do
  mini <- AoC.number
  void (char '-')
  maxi <- AoC.number
  void (char ' ')
  chr <- satisfy isLetter
  void (char ':')
  void (char ' ')
  pass <- takeWhile1P Nothing (/= '\n') <* (eol <|> eof $> "")
  return (mini, maxi, chr, pass)

freq :: String -> [(Char, Int)]
freq xs = map (\x -> (head x, length x)) $ group $ sort xs

findChar :: [(Char, Int)] -> Char -> Int
findChar xs c = snd $ head $ filter (\x -> fst x == c) xs

pInput :: Parser [Line]
pInput = some pLine <* eof

validA :: Line -> Bool
validA (mi, ma, c, p) = any ((== c) . fst) f && cf >= mi && cf <= ma
  where
    f = freq p
    cf = findChar f c

validB :: Line -> Bool
validB (ith, jth, c, p) = (p !! (ith - 1) == c) /= (p !! (jth - 1) == c)

partA :: [Line] -> Int
partA input = length $ filter (== True) $ map validA input

partB :: [Line] -> Int
partB input = length $ filter (== True) $ map validB input

main :: IO ()
main = do
  input <- AoC.getInput 2 pLine
  print (partA input)
  print (partB input)
