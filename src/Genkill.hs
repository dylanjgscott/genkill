{-# LANGUAGE ScopedTypeVariables #-}

module Genkill where

import Data.List

data Graph a = Graph [Node a] [Edge a]
             deriving (Eq, Show)
data Node a  = Node a
             deriving (Eq, Show)
data Edge a  = Edge (Node a, Node a)
             deriving (Eq, Show)

type Gen a b = Graph a -> Node a -> [b]
type Kill a b = Graph a -> Node a -> [b]

-- Generate all the in and out labels for all the nodes in a graph.
genkill
    :: forall a b . (Eq a, Eq b)
    => Graph a
    -> Gen a b
    -> Kill a b
    -> [(Node a, [b], [b])]

genkill g@(Graph ns es) gen kill = [(n, labelin n, labelout n) | n <- ns]

    where

    -- Generates all the labels coming in to a node.
    labelin :: Node a -> [b]
    labelin n = foldl union [] [labelout p | p <- pred es n]

    -- Generates all the labels coming out out of a node.
    labelout :: Node a -> [b]
    labelout n = union (gen g n) ((labelin n) \\ (kill g n))

    -- Lists all the nodes with an edge ending at the given node.
    pred :: [Edge a] -> Node a -> [Node a]
    pred [] _ = []
    pred ((Edge (src, dst)):es) n
        | n == dst = src : pred es n
        | otherwise = pred es n
