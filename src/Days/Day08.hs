{-# LANGUAGE OverloadedStrings #-}

module Days.Day08 where

import qualified AoC
import qualified Data.Set as Set
import qualified Data.Text as T

data State = State
  { accumulator :: Int,
    pointer :: Int,
    executed :: Set.Set Int
  }
  deriving (Show)

initialState :: State
initialState = State 0 0 Set.empty

hasExecuted :: State -> Bool
hasExecuted (State _ ptr ex) = Set.member ptr ex

data Ins
  = NOP Int
  | ACC Int
  | JMP Int
  deriving (Show, Eq)

parseLine :: T.Text -> Ins
parseLine l = case T.splitOn " " l of
  ["nop", xs] -> NOP $ parseNum xs
  ["acc", xs] -> ACC $ parseNum xs
  ["jmp", xs] -> JMP $ parseNum xs
  _ -> error "unreachable"
  where
    parseNum n = read . T.unpack . T.dropWhile (== '+') . T.strip $ n

eval :: Ins -> State -> State
eval (NOP _) (State acc ptr ex) = State acc (ptr + 1) (Set.insert ptr ex)
eval (ACC inc) (State acc ptr ex) = State (acc + inc) (ptr + 1) (Set.insert ptr ex)
eval (JMP off) (State acc ptr ex) = State acc (ptr + off) (Set.insert ptr ex)

getNextIns :: State -> [a] -> a
getNextIns (State _ ptr _) xs = xs !! ptr

execute :: [Ins] -> State -> State
execute ins state
  | hasExecuted state = state
  | otherwise = execute ins (eval (getNextIns state ins) state)

partA :: [Ins] -> Int
partA ins = accumulator $ execute ins initialState

findValid :: [Ins] -> State -> (Bool, State)
findValid ins state
  | hasExecuted state = (False, state)
  | finishedExecution ins state = (True, state)
  | otherwise = findValid ins (eval (getNextIns state ins) state)

switchIns :: Ins -> Ins
switchIns (JMP off) = NOP off
switchIns (NOP c) = JMP c
switchIns i = i

finishedExecution :: [Ins] -> State -> Bool
finishedExecution ins (State _ ptr _) = ptr == length ins

permutate :: [Ins] -> [[Ins]]
permutate xs = map (perm xs) [1 .. length xs]
  where
    perm ys n = take (n - 1) ys ++ [switchIns $ ys !! (n - 1)] ++ drop n ys

partB :: [Ins] -> Int
partB ins = accumulator $ snd $ head $ filter fst $ map (`findValid` initialState) (permutate ins)

main :: IO ()
main = do
  input <- map (parseLine . T.pack) . lines <$> readFile (AoC.inputName 8)
  print (partA input)
  print (partB input)
