{-# LANGUAGE ScopedTypeVariables #-}

module Deadcode where

import Data.List

import Cfg
import Genkill
import Assembly

type Trans a b = [(a, [b], [b])] -> [a] -> [a]

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
deadcodeTrans _ bs = bs

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x
    | fx == x = x
    | otherwise = fixpoint f (fx)
    where fx = f x

deadcode =fixpoint (helper makeCfg union deadcodeGen deadcodeKill deadcodeTrans)

helper :: forall a b . (Eq a, Eq b)
       => MakeCfg a
       -> Comb b
       -> Gen a b
       -> Kill a b
       -> Trans a b
       -> [a]
       -> [a]
helper makeCfg comb gen kill trans x = trans (genkill' x) x
    where
    genkill' = genkill cfg comb gen kill
    cfg = makeCfg x
