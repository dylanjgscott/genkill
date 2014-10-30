module RedReg where 

import Data.List
import Data.Maybe

import Cfg
import Genkill
import Assembly
import Util

data LoadLabel a b = LoadLabel (a, b)

-- [(val, reg1)]

-- [(reg2, reg1)]

-- reg2 is eliminated

-- Turns a list of blocks into a graph
makeCfg :: [Block] -> Cfg ((Integer, Integer), Instruction)
makeCfg bs = 
    let
        blockToNodes (Block id instructs) = map (\x -> CfgNode x)  (zip (zip [id,id..] [0..]) instructs)
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


-- loosely compared tuple

-- (a,b) == (a,c)
-- (a,b) == (c,b)

--gen :: Gen Instruction [(Istruction, Integer)]

--kill :: Gen Instruction

--redreg = fixpoint (runGenKill )

---- makeCfg meet gen kill trans direction x