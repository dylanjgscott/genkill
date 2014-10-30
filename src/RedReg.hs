module RedReg where 

import Data.List
import Data.Maybe

import Cfg
import Genkill
import Assembly
import Fixpoint

-- [(val, reg1)]

-- [(reg2, reg1)]

-- reg2 is eliminated

-- Turns a list of blocks into a graph
makeCfg :: [Block] -> Cfg (Int, (Int, Instruction))
makeCfg bs = 
    let
        blockToNodes (Block id instructs) = map (\x -> CfgNode x) . zip (zip [id,id..] [0..]) instructs
        blocksToNodes = foldl (++ . blockToNodes) []
        nodes = blocksToNodes bs

        successors (CfgNode x) = successors' x


        successors' ((blk, ln), (Br _ blk1 blk2)) = successors'' (blk, ln + 1) 
                                                 ++ successors'' (blk1, 0) 
                                                 ++ successors'' (blk2, 0)
        successors' ((blk, ln), _) = successors'' (blk, ln + 1)


        successors'' idf = map isJust (find (\(CfgNode x) -> fst x == idf) nodes)

        nodeToEdges node = map (\x -> CfgEdge) (zip [node,node..] (successors node))

        edges = foldl (++ . nodeToEdges)  [] nodes

--gen :: Gen Instruction [(Istruction, Integer)]

--kill :: Gen Instruction

--redreg = fixpoint (runGenKill )

---- makeCfg meet gen kill trans direction x