module Lib where

import Data.List (sortBy, insertBy)
import Data.Char (chr)
import Data.Maybe (fromJust)

-- for convenience
log2 = logBase 2

entropy :: [Double] -> Double
entropy probs
  = sum (map entropy' probs)
  where
    entropy' :: Double -> Double
    entropy' 0.0
      = 0.0
    entropy' prob
      = - prob * log2 prob

kraftMcMillanNumber :: [(Char, String)] -> Double
kraftMcMillanNumber codes
  = sum (map (\(_, encoded) -> 1.0 / fromIntegral (length encoded)) codes)

averageWordLength :: [(Double, String)] -> Double
averageWordLength codes
  = sum (map (\(prob, encoded) -> prob * fromIntegral (length encoded)) codes)

shannonFanoAWL :: [Double] -> Double
shannonFanoAWL probs
  = sum (map (\prob -> - prob * fromIntegral (floor (log2 prob))) probs)

data Tree a b
  = Empty
  | Leaf a b
  | Node (Tree a b) b (Tree a b)

huffmanAWL :: [Double] -> Double
huffmanAWL probs
  = sum [findProb symb * fromIntegral (length code) | (symb, code) <- encoded]
  where
    codes       = zip (map chr [0..]) probs
    findProb symb = fromJust (lookup symb codes)
    encoded     = huffman codes

huffman :: [(Char, Double)] -> [(Char, String)]
huffman
  = codeify . buildTree
  where
    codeify :: Tree Char Double -> [(Char, String)]
    codeify Empty
      = []
    codeify (Leaf code _)
      = [(code, "")]
    codeify (Node left _ right)
      = appendPrefix '0' (codeify left) ++ appendPrefix '1' (codeify right)
      where
        appendPrefix :: Char -> [(Char, String)] -> [(Char, String)]
        appendPrefix prefix
          = map (\(code, encoded) -> (code, prefix : encoded))
    treeComp :: Tree Char Double -> Tree Char Double -> Ordering
    treeComp Empty Empty
      = EQ
    treeComp (Leaf _ val1) (Leaf _ val2)
      = compare val1 val2
    treeComp (Node _ val1 _) (Node _ val2 _)
      = compare val1 val2
    treeComp Empty (Leaf _ val)
      = compare 0.0 val
    treeComp (Leaf _ val) Empty
      = compare val 0.0
    treeComp Empty (Node _ val _)
      = compare 0.0 val
    treeComp (Node _ val _) Empty
      = compare val 0.0
    treeComp (Leaf _ val1) (Node _ val2 _)
      = compare val1 val2
    treeComp (Node _ val1 _) (Leaf _ val2)
      = compare val1 val2
    buildTree :: [(Char, Double)] -> Tree Char Double
    buildTree probs
      = mergeTrees (sortBy treeComp (map makeLeaf probs))
      where
        makeLeaf (char, prob)
          = Leaf char prob
    mergeTrees []
      = Empty
    mergeTrees [tree]
      = tree
    mergeTrees (tree1: tree2: trees)
      = mergeTrees (insertBy treeComp (mergeTree tree1 tree2) trees)
    mergeTree Empty leaf@(Leaf _ _)
      = leaf
    mergeTree Empty node@(Node _ _ _)
      = node
    mergeTree leaf1@(Leaf _ prob1) leaf2@(Leaf _ prob2)
      = Node leaf1 (prob1 + prob2) leaf2
    mergeTree leaf@(Leaf _ prob1) node@(Node _ prob2 _)
      = Node leaf (prob1 + prob2) node
    mergeTree node1@(Node _ prob1 _) node2@(Node _ prob2 _)
      = Node node1 (prob1 + prob2) node2
    mergeTree t1 t2
      = mergeTree t2 t1
