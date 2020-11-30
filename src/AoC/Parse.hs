-- |
-- Module      : AoC.Parse
-- Description : Utility functions for parsing problems and their inputs.
module AoC.Parse where

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
inputName = printf "input/day%02d.txt"

-- | Returns the name of the example inputs for a day and part.
exampleName ::
  -- | day
  Int ->
  -- | part
  Char ->
  FilePath
exampleName = printf "inputs/day%02d%c.txt"

type Parser = Parsec Void String

number :: Integral a => Parser a
number = L.signed (return ()) L.decimal

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
