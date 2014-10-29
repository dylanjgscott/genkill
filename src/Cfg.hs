module Cfg where

import Assembly

data Cfg a = Cfg [CfgNode a] [CfgEdge a]
             deriving (Eq, Show)
data CfgNode a  = CfgNode a
             deriving (Eq, Show)
data CfgEdge a  = CfgEdge (CfgNode a, CfgNode a)
             deriving (Eq, Show)

-- Turns a list of blocks into a graph
makeCfg :: [Block] -> Cfg Block
makeCfg bs = Cfg nodes edges
    where

    -- The nodes for the graph
    nodes :: [CfgNode Block]
    nodes = map CfgNode bs

    -- The edges for the graph
    edges :: [CfgEdge Block]
    edges = concat $ map blockCfgEdges bs

    -- Takes a block and returns all the directed edges for it
    blockCfgEdges :: Block -> [CfgEdge Block]
    blockCfgEdges b@(Block _ is) = map (\x -> CfgEdge (CfgNode b, CfgNode x)) (linkedBlocks is)

    -- Return all the possible blocks a set of instructions can branch to
    linkedBlocks :: [Instruction] -> [Block]
    linkedBlocks [] = []
    linkedBlocks (Br _ id1 id2:is) = lookupBlock bs id1 : lookupBlock bs id2 : linkedBlocks is
    linkedBlocks (_:is) = linkedBlocks is


-- Make a graph for each function in a program
makeCfgList :: Program -> [Cfg Block]
makeCfgList = map (makeCfg . getBlocks)

-- Lists all the nodes with an edge ending at the given node.
pred :: Eq a => [CfgEdge a] -> CfgNode a -> [CfgNode a]
pred [] _ = []
pred ((CfgEdge (src, dst)):es) node
    | node == dst = src : Cfg.pred es node
    | otherwise = Cfg.pred es node

-- Lists all the nodes with an edge beginning at the given node.
succ :: Eq a => [CfgEdge a] -> CfgNode a -> [CfgNode a]
succ [] _ = []
succ ((CfgEdge (src, dst)):es) node
    | node == dst = src : Cfg.succ es node
    | otherwise = Cfg.succ es node
