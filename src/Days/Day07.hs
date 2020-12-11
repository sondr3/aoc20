{-# LANGUAGE NamedFieldPuns #-}

module Days.Day07 where

import qualified AoC
import Control.Monad (guard, void)
import Data.List (find, unfoldr)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Semigroup (Sum (..))
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

data Bag = Bag String [(Int, String)] deriving (Show)

pBags :: Parser (Map.Map String [(Int, String)])
pBags = do
  bags <- some pBag <* eof
  return $ Map.fromList $ map bs bags
  where
    bs (Bag color ys) = (color, ys)

pBag :: Parser Bag
pBag = do
  bName <- pBagColor
  bRule <- pRules
  void eol
  return $ Bag bName bRule

pRules :: Parser [(Int, String)]
pRules = do
  empt <- optional $ try (chunk "s contain no other bags.")
  case empt of
    Just _ -> return []
    Nothing -> do
      void (chunk "s contain ")
      many pRule

pRule :: Parser (Int, String)
pRule = do
  num <- read <$> some digitChar
  void (char ' ')
  color <- pBagColor
  void (chunk "s, " <|> chunk ", " <|> chunk "s." <|> chunk ".")
  return (num, color)

pBagColor :: Parser String
pBagColor = manyTill anySingle (chunk " bag")

partA :: Map.Map String [(a, String)] -> Int
partA xs = length $ mapMaybe (findColor "shiny gold" xs) $ Map.keys xs

partB :: (Integral c, Ord b) => Map.Map b [(c, b)] -> b -> c
partB ms k = getSum . foldMap (Sum . fst) $ concat $ unfoldr f [k]
  where
    f keys = case concatMap (\n -> Map.findWithDefault [] n ms) keys of
      [] -> Nothing
      xs -> Just (xs, concat [replicate (fromIntegral c) n | (c, n) <- xs])

findColor :: Ord b => b -> Map.Map b [(a, b)] -> b -> Maybe (a, b)
findColor color ms curr = do
  children <- Map.lookup curr ms
  guard . not $ null children
  find ((==) color . snd) children <|> listToMaybe (mapMaybe (findColor color ms . snd) children)

main :: IO ()
main = do
  input <- readFile (AoC.inputName 7)
  case parse pBags "" input of
    Right out -> do
      print (partA out)
      print (partB out "shiny gold")
    Left err -> putStrLn $ errorBundlePretty err
