module Deadcode where

import Data.List

import Cfg
import Genkill
import Assembly
import Fixpoint

deadcodeGen :: Gen Block Reg
deadcodeGen (Block _ is) = allocatedRegs is
    where
    allocatedRegs :: [Instruction] -> [Reg]
    allocatedRegs [] = []
    allocatedRegs (i:is) = let
        reg = case i of
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
        in
            reg ++ allocatedRegs is

deadcodeKill :: Kill Block Reg
deadcodeKill (Block _ is) = usedRegs is
    where
    usedRegs :: [Instruction] -> [Reg]
    usedRegs [] = []
    usedRegs (i:is) = let
        reg = case i of
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
        in
            reg ++ usedRegs is

deadcodeTrans :: Trans Block Reg
deadcodeTrans _ [] = []
deadcodeTrans _ (b:bs) = bs
    --where
        --transBlock :: Block -> [Reg] -> [Reg] -> Block
        --transBlock block inRegs outRegs
        --
deadcode = fixpoint (helper makeCfg union deadcodeGen deadcodeKill deadcodeTrans)
