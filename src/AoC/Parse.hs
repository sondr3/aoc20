-- |
-- Module      : AoC.Parse
-- Description : Utility functions for parsing problems and their inputs.
module AoC.Parse where

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
