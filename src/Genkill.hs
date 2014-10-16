module Genkill where

import Data.List

data Graph a = Graph [Node a] [Edge a]
             deriving (Eq, Show)

data Node a  = Node a
             deriving (Eq, Show)
data Edge a  = Edge (Node a, Node a)
             deriving (Eq, Show)

nodes = [Node "1y", Node "2x", Node "3z", Node "4x", Node "5x", Node "6"]
edges = [ Edge (nodes!!0, nodes!!1),
    Edge (nodes!!1, nodes!!2),
    Edge (nodes!!1, nodes!!3),
    Edge (nodes!!2, nodes!!5),
    Edge (nodes!!3, nodes!!4),
    Edge (nodes!!4, nodes!!5) ]
graph = Graph nodes edges

gen :: Graph [Char] -> Node [Char] -> [Node [Char]]
gen g@(Graph ns es) n@(Node x) = if length x == 2 then [n] else []

kill :: Graph [Char] -> Node [Char] -> [Node [Char]]
kill g@(Graph ns es) n@(Node x) = if length x == 2 then overlapping (x!!1) (delete n ns) else []
    where
        overlapping :: Char -> [Node [Char]] -> [Node [Char]]
        overlapping _ [] = []
        overlapping c (n@(Node x):ns)
            | length x == 2 && x!!1 == c = n : overlapping c ns
            | otherwise = overlapping c ns

genkill
    :: Eq a
    => Graph a
    -> (Graph a -> Node a -> [Node a])
    -> (Graph a -> Node a -> [Node a])
    -> [(Node a, [Node a], [Node a])]
genkill g@(Graph ns es) gen kill = [(n, labelin n, labelout n) | n <- ns]
    where
    labelin n = foldl union [] [labelout p | p <- pred es n]
    labelout n = union (gen g n) ((labelin n) \\ (kill g n))
    pred [] _ = []
    pred ((Edge (src, dst)):es) n
        | n == dst = src : pred es n
        | otherwise = pred es n
