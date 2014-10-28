module Unreachable where

import Data.List

import Cfg
import Genkill
import Assembly
import Fixpoint

unreachableGen :: Gen Block Reg
unreachableGen (Block _ is) = allocatedRegs is
    where
    allocatedRegs :: [Instruction] -> [Reg]
    allocatedRegs _ = []


unreachableKill :: Kill Block Reg
unreachableKill (Block _ is) = usedRegs is
    where
    usedRegs :: [Instruction] -> [Reg]
    usedRegs [] = []
    usedRegs (i:is) = let
        reg = case i of
            (Br r _ _) -> [r]
            (Ret r) -> [r]
            otherwise -> []
        in
            reg ++ usedRegs is

unreachableTrans :: Trans Block Reg
unreachableTrans _ [] = []
unreachableTrans (a:as) (b:bs) 
    | fst (snd a) == [] && (getBlockNum b) /= 0 = unreachableTrans as bs
    | otherwise = [b] ++ unreachableTrans as bs
 
unreachable = fixpoint (helper makeCfg union unreachableGen unreachableKill unreachableTrans)

getBlockNum :: Block -> Integer
getBlockNum (Block num _ ) = num
