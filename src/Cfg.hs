module Cfg where

import Assembly
import Util

data Cfg a = Cfg [CfgNode a] [CfgEdge a]
             deriving (Eq, Show)
data CfgNode a  = CfgNode a
             deriving (Eq, Show)
data CfgEdge a  = CfgEdge (CfgNode a, CfgNode a)
             deriving (Eq, Show)


type MakeCfg a b = [a] -> Cfg b

-- Make a graph for each function in a program
makeCfgList :: ([Block] -> Cfg a) -> Program -> [Cfg a]
makeCfgList makeFun = map (makeFun . getBlocks)

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


-- Make graph functions

-- type alais for instruction based nodes
type InstrNode = ((Integer, Integer), Instruction)

-- Turns a list of blocks into a graph
-- where a node is an instruction (with an additional
-- lookup so that the node can be unquietly linked to
-- an instruction in the program
makeInstrCfg :: [Block] -> Cfg InstrNode
makeInstrCfg bs = 
    let
        -- Convert a single block into a list of nodes
        blockToNodes (Block blk instructs) = map (\x -> CfgNode x)  (mapBlockToInstr blk instructs)
        
        -- Apply the single block transform to every block in the blocklist
        blocksToNodes = foldl (\x y -> x ++ (blockToNodes y)) []

        -- Generate the graph nodes
        nodes = blocksToNodes bs


        -- unpack node in order to find successors 
        successors (CfgNode x) = successors' x

        -- successors are either the direct next instruction or
        -- the next instruction and the first instruction in any block
        -- that a branch links too.
        successors' ((blk, ln), (Br _ blk1 blk2)) = getNodes (blk, ln + 1) 
                                                 ++ getNodes (blk1, 0)
                                                 ++ getNodes (blk2, 0)
        successors' ((blk, ln), _) = getNodes (blk, ln + 1)


        -- get the node/s given a lookup pair (block, line)
        getNodes idf = filter (\(CfgNode x) -> fst x == idf) nodes


        -- An infinite list of a single node
        -- useful for following zip of node to all its successors
        infNode node = node : infNode node

        -- creates all the edges for a given node and its successors
        nodeToEdges node = map (\x -> CfgEdge x) (zip (infNode node) (successors node))

        -- traverse graph and generate all edges
        edges = foldl (\x y -> x ++ (nodeToEdges y))  [] nodes
    in
    Cfg nodes edges


-- Turns a list of blocks into a graph
makeBlockCfg :: [Block] -> Cfg Block
makeBlockCfg bs = Cfg nodes edges
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
