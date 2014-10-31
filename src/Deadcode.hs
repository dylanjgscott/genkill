module Deadcode where

import Data.List
import Data.Maybe

import Cfg
import Genkill
import Assembly
import Util

-- Return a list of all the registers used by an instruction
use :: InstrNode -> [Reg]
use ((_, _), instr) = case instr of
    (St _ r) -> [r]
    (Add _ r1 r2) -> [r1, r2]
    (Sub _ r1 r2) -> [r1, r2]
    (Mul _ r1 r2) -> [r1, r2]
    (Div _ r1 r2) -> [r1, r2]
    (Lt _ r1 r2) -> [r1, r2]
    (Gt _ r1 r2) -> [r1, r2]
    (Eq _ r1 r2) -> [r1, r2]
    (Br r _ _) -> [r]
    (Ret r) -> [r]
    (Call _ _ rs) -> rs
    otherwise -> []

-- Return a list of all the registers defined by an instruction
def :: InstrNode -> [Reg]
def ((_, _), instr) = case instr of
    (Lc r _) -> [r]
    (Ld r _) -> [r]
    (Add r _ _) -> [r]
    (Sub r _ _) -> [r]
    (Mul r _ _) -> [r]
    (Div r _ _) -> [r]
    (Lt r _ _) -> [r]
    (Gt r _ _) -> [r]
    (Eq r _ _) -> [r]
    (Call r _ _) -> [r]
    otherwise -> []

-- Remove dead code from a list of blocks
deadcodeTrans :: Transform Block InstrNode Reg
deadcodeTrans flowdata blocks = map (removeDeadcode flowdata) blocks

-- Remove dead code from a single block
removeDeadcode :: Labels InstrNode Reg -> Block -> Block
removeDeadcode flowdata (Block bknum instrs) = Block bknum liveInstrs
    where

    -- List of live instructions in this block
    liveInstrs :: [Instruction]
    liveInstrs = map unpackInstr (filter isLive packedInstrs)

    -- List of live registers during the execution of an instruction
    liveRegs :: InstrNode -> [Reg]
    liveRegs x = snd . fromJust $ lookup x flowdata

    -- Returns true only if an instruction is live
    isLive :: InstrNode -> Bool
    isLive instr 
        | def instr == [] = True
        | all (\x -> x `notElem` (liveRegs instr)) (def instr) = False
        | otherwise = True

    -- Unpack an instruction from the graph type
    unpackInstr :: InstrNode -> Instruction 
    unpackInstr ((_, _), i) = i

    -- A list of packed instructions to match the graph exactly
    packedInstrs :: [InstrNode]
    packedInstrs = zip (zip [bknum,bknum..] [0..]) instrs


-- Apply the deadcode removal framework using fixpoint
deadcodeBlockTransform :: [Block] -> [Block]
deadcodeBlockTransform = fixpoint (runGenKill makeInstrCfg union use def deadcodeTrans Backwards)

-- Apply the deadcode removal to each function in a program
deadcode :: Program -> Program
deadcode = applyBlockTransform deadcodeBlockTransform
