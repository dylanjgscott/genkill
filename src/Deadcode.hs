module Deadcode where

import Data.List
import Data.Maybe

import Cfg
import Genkill
import Assembly
import Fixpoint

use :: Gen Block Reg
use (Block _ is) = foldl union [] (map usedRegs is)

def :: Kill Block Reg
def (Block _ is) = foldl union [] (map definedRegs is)

usedRegs :: Instruction -> [Reg]
usedRegs reg = case reg of
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

definedRegs :: Instruction -> [Reg]
definedRegs reg = case reg of
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

deadcodeTrans :: Trans Block Reg
deadcodeTrans flowdata [] = []
deadcodeTrans flowdata (b:bs) = deleteDeadcode b usedRegs : deadcodeTrans flowdata bs
    where
    usedRegs = fst . fromJust $ lookup b flowdata

deleteDeadcode :: Block -> [Reg] -> Block
deleteDeadcode (Block num ins) liveRegs = Block num liveIns
    where
    liveIns :: [Instruction]
    liveIns = filter isAlive ins
    isAlive :: Instruction -> Bool
    isAlive i = (definedRegs i) \\ liveRegs == []
        
deadcodeBlockTransform :: [Block] -> [Block]
deadcodeBlockTransform = fixpoint (applyTransformation makeCfg union use def deadcodeTrans Backwards)

deadcode :: Program -> Program
deadcode p = applyBlockTransform deadcodeBlockTransform p
