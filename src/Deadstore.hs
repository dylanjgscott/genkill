module Deadstore where

import Data.List
import Data.Maybe

import Cfg
import Genkill
import Assembly
import Util

-- Return a list of all the variables used by an instruction
use :: InstrNode -> [Id]
use ((_, _), instr) = case instr of
    (Ld _ x) -> [x]
    otherwise -> []

-- Return a list of all the variables defined by an instruction
def :: InstrNode -> [Id]
def ((_, _), instr) = case instr of
    (St x _) -> [x]
    otherwise -> []

-- Remove dead code from a list of blocks
deadstoreTrans :: Transform Block InstrNode Id
deadstoreTrans flowdata blocks = map (removeDeadstore flowdata) blocks

-- Remove dead code from a single block
removeDeadstore :: Labels InstrNode Id -> Block -> Block
removeDeadstore flowdata (Block bknum instrs) = Block bknum liveInstrs
    where

    -- List of live instructions in this block
    liveInstrs :: [Instruction]
    liveInstrs = map unpackInstr (filter isLive packedInstrs)

    -- List of dead registers during the execution of an instruction
    liveRegs :: InstrNode -> [Id]
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


-- Apply the dead store instruction removal framework using fixpoint
deadstoreBlockTransform :: [Block] -> [Block]
deadstoreBlockTransform = fixpoint (runGenKill makeInstrCfg union use def deadstoreTrans Backwards)

-- Apply the dead store instruction removal to each function in a program
deadstore :: Program -> Program
deadstore = applyBlockTransform deadstoreBlockTransform
