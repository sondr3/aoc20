-- |
-- Module      : AoC.Parse
-- Description : Utility functions for parsing problems and their inputs.
module AoC.Parse where

import Control.Exception (throw)
import Control.Monad (void)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Functor (($>))
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Printf (printf)

-- | Returns the name of the input file for a given day.
inputName ::
  -- | day
  Int ->
  FilePath
inputName = printf "inputs/day%02d.txt"

-- | Returns the name of the example inputs for a day and part.
exampleName ::
  -- | day
  Int ->
  -- | part
  Char ->
  FilePath
exampleName = printf "inputs/day%02d%c.txt"

getInput :: Int -> Parser a -> IO [a]
getInput i p = do
  input <- readFile (inputName i)
  pure $ pLines p input

getExampleInput :: Int -> Char -> Parser a -> IO [a]
getExampleInput i c p = do
  input <- readFile (exampleName i c)
  pure $ pLines p input

type Parser = Parsec Void String

number :: Integral a => Parser a
number = L.signed (return ()) L.decimal

pLines :: Parser a -> String -> [a]
pLines parser input = case parse (traverse p (lines input)) "" input of
  Left err -> throw err
  Right a -> a
  where
    p l = setInput l *> parser <* eof <* setInput "\n" <* newline

exampleToNum :: (String, String) -> (Int, Int)
exampleToNum = bimap read read

pExamples :: Parser [(String, String)]
pExamples = some pExample <* eof

pExample :: Parser (String, String)
pExample = do
  input <- concat <$> some pInput
  answer <- pAnswer
  pure (input, answer)
  where
    pInput = do
      notFollowedBy (chunk ">>>" <|> eof $> "")
      takeWhile1P Nothing (/= '\n') <> (eol <|> eof $> "")
    pAnswer = do
      void (chunk ">>>")
      takeWhile1P Nothing (/= '\n') <* (eol <|> eof $> "")
