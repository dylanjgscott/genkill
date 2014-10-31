module RedReg where 

import Data.List
import Data.Maybe

import Cfg
import Genkill
import Assembly
import Util

data LoadLabel = LoadLabel (Reg, Id)
               deriving (Show)


instance Eq LoadLabel where
    (LoadLabel (x,y)) == (LoadLabel (a,b)) = (x == a || y == b)

-- [(val, reg1)]

-- [(reg2, reg1)]

-- reg2 is eliminated

gen :: Gen InstrNode LoadLabel
gen ((blk, ln), instr) = case instr of
    (Ld reg idf) -> [LoadLabel (reg, idf)]
    (Lc reg cons) -> [LoadLabel(reg, show cons)]
    otherwise -> []

kill :: Gen InstrNode LoadLabel
kill ((blk, ln), instr) = case instr of
    (St idf reg) -> [LoadLabel (reg, idf)]
    (Add reg _ _) -> [LoadLabel (reg, "")]
    (Sub reg _ _) -> [LoadLabel (reg, "")]
    (Mul reg _ _) -> [LoadLabel (reg, "")]
    (Div reg _ _) -> [LoadLabel (reg, "")]
    (Call reg _ _) -> [LoadLabel (reg, "")]
    otherwise -> []




redregTrans :: Transform Block InstrNode LoadLabel
redregTrans _ [] = []
redregTrans labels blocks = map (redregTrans' labels) blocks

redregTrans' :: Labels InstrNode LoadLabel -> Block -> Block
redregTrans' labels (Block blk instructs) =
    let
        transformed = map (redregTrans'' labels) (zip (zip [blk,blk..] [0..]) instructs)
    in
        Block blk transformed

redregTrans'' :: Labels InstrNode LoadLabel -> InstrNode -> Instruction
redregTrans'' labels n@(idf, instr) = 
    let
        -- We know that is must exist in labels, otherwise a node was not
        -- properly generated in the graph, in which case failing hard
        -- is acceptable as it indicates a serious logic error
        (ins, outs) = fromJust (lookup n labels)

        --newInstr = if not (null ins) then (Ld 0 "changed") else instr
        newInstr = case instr of
            (St idf reg) -> St idf (getLowestReg ins reg)
            (Add reg1 reg2 reg3) -> Add reg1 (getLowestReg ins reg2) (getLowestReg ins reg3)
            (Sub reg1 reg2 reg3) -> Sub reg1 (getLowestReg ins reg2) (getLowestReg ins reg3)
            (Mul reg1 reg2 reg3) -> Mul reg1 (getLowestReg ins reg2) (getLowestReg ins reg3)
            (Div reg1 reg2 reg3) -> Div reg1 (getLowestReg ins reg2) (getLowestReg ins reg3)
            (Ret reg) -> Ret (getLowestReg ins reg)
            (Call reg idf regs) -> (Call reg idf (map (getLowestReg ins) regs)) 
            otherwise -> instr
    in
        newInstr

getLowestReg :: [LoadLabel] -> Reg -> Reg
getLowestReg labels reg = 
    if isJust reglab 
    then (sort (map (fst . unpack) (filter (filterSecond (snd (unpack (fromJust reglab)))) labels))) !! 0
    else reg
    where 
    filterFirst  r (LoadLabel pr) = fst pr == r
    filterSecond v (LoadLabel pr) = snd pr == v
    reglab = find (filterFirst reg) labels
    unpack (LoadLabel x) = x

tmpkill cfg = genkill cfg union gen kill Forwards

redreg = applyBlockTransform (fixpoint (runGenKill makeInstrCfg intersect gen kill redregTrans Forwards))

-- loosely compared tuple

-- (a,b) == (a,c)
-- (a,b) == (c,b)

--gen :: Gen Instruction [(Istruction, Integer)]

--kill :: Gen Instruction

--redreg = fixpoint (runGenKill )

---- makeCfg meet gen kill trans direction x