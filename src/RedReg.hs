module RedReg where 

import Data.List
import Data.Maybe

import Cfg
import Genkill
import Assembly
import Util

-- Custom label type is used in order to have correct gen
-- and kill behavior
data LoadLabel = LoadLabel (Reg, Id)
               deriving (Show)

instance Eq LoadLabel where
    (LoadLabel (x,y)) == (LoadLabel (a,b)) =
        -- if the  register and the values match they are identical 
        ((x == a) && (y == b))
        -- if the registers match and one of the values is empty
        -- they are the same (this ensures killing of all of a register
        -- duplicated in an input list)
        || ((x == a) && ( (y == "") || (b == "")))
        -- if both values match and one register is 0 (an invalid placeholder)
        -- they are the same (this ensures killing of all of a value
        -- duplicated in an input list)
        || (((x == 0 ) || (a == 0)) && (y == b)) 


-- Each time a register has a value either loaded into it
-- or a register value is stored in a variable
-- generate an association between the register and the value/var
gen :: Gen InstrNode LoadLabel
gen ((blk, ln), instr) = case instr of
    (Ld reg idf) -> [LoadLabel (reg, idf)]
    (Lc reg cons) -> [LoadLabel(reg, show cons)]
    (St idf reg) -> [LoadLabel (reg, idf)]
    otherwise -> []

-- If a register is the destination of a computation
-- kill all subsequent value pairs of a register ("")
-- dummy value is used in order to match all registers pairs.
-- Similarly if a var has its value changed, kill
-- all the register var pairs from the output of this node
-- (0) dummy register is used to match all values pairs.
kill :: Gen InstrNode LoadLabel
kill ((blk, ln), instr) = case instr of
    (St idf reg) -> [LoadLabel (0, idf)]
    (Ld reg idf) -> [LoadLabel (reg, "")]
    (Lc reg cons) -> [LoadLabel (reg, "")]
    (Add reg _ _) -> [LoadLabel (reg, "")]
    (Sub reg _ _) -> [LoadLabel (reg, "")]
    (Mul reg _ _) -> [LoadLabel (reg, "")]
    (Div reg _ _) -> [LoadLabel (reg, "")]
    (Call reg _ _) -> [LoadLabel (reg, "")]
    otherwise -> []



-- Applies block level optimizations to every block in a function
redregTrans :: Transform Block InstrNode LoadLabel
redregTrans _ [] = []
redregTrans labels blocks = map (redregTrans' labels) blocks

-- Recreates unique mapping from blocks to instruction where each instruction
-- is indexed by (block number, line number). This is required as it the the
-- same representation that the makeInstrCfg uses and as such in order to associate
-- an individual instruction with a node in the graph in order to get its in and out labels
-- the mapping has to be regenerated
redregTrans' :: Labels InstrNode LoadLabel -> Block -> Block
redregTrans' labels (Block blk instructs) =
    let
        -- Use of infinite lists in order to concisely generate mapping
        -- this is the same code as is used in makeInstrCfg and has extracted
        -- into a separate function in util
        transformed = map (redregTrans'' labels) (mapBlockToInstr blk instructs)
    in
        Block blk transformed

redregTrans'' :: Labels InstrNode LoadLabel -> InstrNode -> Instruction
redregTrans'' labels n@(idf, instr) = 
    let
        -- We know that is must exist in labels, otherwise a node was not
        -- properly generated in the graph, in which case failing hard
        -- is acceptable as it indicates a serious logic error
        (ins, outs) = fromJust (lookup n labels)

        -- Now we look at the in labels for each node and due to the way
        -- we generated them using gen kill, we can assume that they are valid
        -- in all contexts of reaching this instruction (this is due to intersect
        -- being the meet function for redundant load elimination). We then rewrite
        -- any register being used as a source (as opposed to a destination) in an instruction
        -- with the lowest lexical equivalent based on value stored.
        newInstr = case instr of
            (St idf reg) -> St idf (getLowestReg ins reg)
            (Add reg1 reg2 reg3) -> Add reg1 (getLowestReg ins reg2) (getLowestReg ins reg3)
            (Sub reg1 reg2 reg3) -> Sub reg1 (getLowestReg ins reg2) (getLowestReg ins reg3)
            (Mul reg1 reg2 reg3) -> Mul reg1 (getLowestReg ins reg2) (getLowestReg ins reg3)
            (Div reg1 reg2 reg3) -> Div reg1 (getLowestReg ins reg2) (getLowestReg ins reg3)
            (Eq reg1 reg2 reg3) -> Eq reg1 (getLowestReg ins reg2) (getLowestReg ins reg3)
            (Lt reg1 reg2 reg3) -> Lt reg1 (getLowestReg ins reg2) (getLowestReg ins reg3)
            (Gt reg1 reg2 reg3) -> Gt reg1 (getLowestReg ins reg2) (getLowestReg ins reg3)
            (Br reg blk1 blk2) -> Br (getLowestReg ins reg) blk1 blk2
            (Ret reg) -> Ret (getLowestReg ins reg)
            (Call reg idf regs) -> (Call reg idf (map (getLowestReg ins) regs)) 
            otherwise -> instr
    in
        newInstr

getLowestReg :: [LoadLabel] -> Reg -> Reg
getLowestReg labels reg = 
    if isJust reglab 
    -- Extract all the registers with identical values based on the input label list
    -- sort them lexicographically and pick the first of the sorted list as the canonical
    -- correct register if they exist, otherwise just return the queried register
    then (sort (map (fst . unpack) (filter (filterSecond (snd (unpack (fromJust reglab)))) labels))) !! 0
    else reg
    where 
    filterFirst  r (LoadLabel pr) = fst pr == r
    filterSecond v (LoadLabel pr) = snd pr == v
    -- find a registers value based on the input labels
    reglab = find (filterFirst reg) labels
    unpack (LoadLabel x) = x


-- The nodes of the genkill graph are instructions as such the makeInstrCfg graph builder is used.
-- The redundant load gen and kill functions are used in the genkill function.
-- The redundant load transform function is used to modify the instructions.
-- Intersect is used to ensure that the graph in nodes are true in all input sets to an instruction
-- The genkill function operated in the forward directions along edges
-- applyBlockTransform is used to ensure the type of redreg is from Program -> Program
-- rather than [Block] -> [Block]
redreg = applyBlockTransform (fixpoint (runGenKill makeInstrCfg intersect gen kill redregTrans Forwards))