module Cfg where

import Assembly

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
    | node == src = dst : Cfg.succ es node
    | otherwise = Cfg.succ es node


-- Make graph functions

-- type alais for instruction based nodes
type InstrNode = ((Integer, Integer), Instruction)

-- Turns a list of blocks into a graph
makeInstrCfg :: [Block] -> Cfg InstrNode
makeInstrCfg bs = 
    let
        blockToNodes (Block blk instructs) = map (\x -> CfgNode x)  (zip (zip [blk,blk..] [0..]) instructs)
        blocksToNodes = foldl (\x y -> x ++ (blockToNodes y)) []
        nodes = blocksToNodes bs

        infNode node = node : infNode node

        successors (CfgNode x) = successors' x


        successors' ((blk, ln), (Br _ blk1 blk2)) = getNodes (blk, ln + 1) 
                                                 ++ getNodes (blk1, 0)
                                                 ++ getNodes (blk2, 0)
        successors' ((blk, ln), _) = getNodes (blk, ln + 1)


        getNodes idf = filter (\(CfgNode x) -> fst x == idf) nodes

        nodeToEdges node = map (\x -> CfgEdge x) (zip (infNode node) (successors node))

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
