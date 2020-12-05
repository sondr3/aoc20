module AoC.BST where

import qualified Data.Tree as T

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

treeFromList :: [a] -> Tree a
treeFromList [] = Leaf
treeFromList xs = Node (treeFromList start) (head end) (treeFromList (tail end))
  where
    (start, end) = splitAt ((length xs `div` 2) - 1) xs

treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert val Leaf = Node Leaf val Leaf
treeInsert val (Node left key right)
  | key == val = Node left val right
  | key < val = Node (treeInsert val left) key right
  | otherwise = Node left key (treeInsert val right)

treeContains :: Ord a => Tree a -> a -> Bool
treeContains Leaf _ = False
treeContains (Node left key right) val
  | key == val = True
  | key < val = treeContains right val
  | otherwise = treeContains left val

treeSize :: Tree a -> Int
treeSize Leaf = 0
treeSize (Node left _ right) = 1 + treeSize left + treeSize right

treeRoot :: Tree a -> Maybe a
treeRoot Leaf = Nothing
treeRoot (Node _ x _) = Just x

treeMin :: Tree a -> Maybe a
treeMin Leaf = Nothing
treeMin (Node Leaf x _) = Just x
treeMin (Node left _ _) = treeMin left

treeMax :: Tree a -> Maybe a
treeMax Leaf = Nothing
treeMax (Node _ x Leaf) = Just x
treeMax (Node _ _ right) = treeMax right

drawTree :: Show a => Tree a -> String
drawTree t = T.drawTree $ toDataTree t
  where
    toDataTree :: (Show a) => Tree a -> T.Tree String
    toDataTree Leaf = T.Node "" []
    toDataTree (Node left val right) = T.Node (show val) [toDataTree left, toDataTree right]
