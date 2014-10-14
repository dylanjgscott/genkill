module Genkill where

import Data.List

data Graph a = Graph [Node a] [Edge a]
             deriving (Eq, Show)

data Node a  = Node a
             deriving (Eq, Show)
data Edge a  = Edge (Node a, Node a)
             deriving (Eq, Show)

nodes = [Node "test1", Node "test2", Node "test3"]
edges = [Edge (nodes!!0, nodes!!1), Edge (nodes!!0, nodes!!2)]
graph = Graph nodes edges

gen :: Eq a => Graph a -> Node a -> [Node a]
gen g n = [n]

kill :: Eq a => Graph a -> Node a -> [Node a]
kill g n = []

genkill
    :: Eq a
    => Graph a
    -> (Graph a -> Node a -> [Node a])
    -> (Graph a -> Node a -> [Node a])
    -> [(Node a, [Node a], [Node a])]
genkill g@(Graph ns es) gen kill = [(n, labelin g n, labelout g n) | n <- ns]

labelin :: Eq a => Graph a -> Node a -> [Node a]
labelin g@(Graph ns es) n = foldl union [] [labelout g p | p <- predecessors es n]

labelout :: Eq a => Graph a -> Node a -> [Node a]
labelout g@(Graph ns es) n = union (gen g n) ((labelin g n) \\ (kill g n))

predecessors :: Eq a => [Edge a] -> Node a -> [Node a]
predecessors [] _ = []
predecessors ((Edge (src, dst)):es) n
    | n == dst = src : predecessors es n
    | otherwise = predecessors es n
