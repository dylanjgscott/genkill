module Unreachable where

import Data.List

import Cfg
import Genkill
import Assembly
import Fixpoint

unreachableGen :: Gen Block Bool
unreachableGen (Block num _) 
    | num == 0 = [True]
    | otherwise = []

unreachableKill :: Kill Block Bool
unreachableKill _ = []

unreachableTrans :: Trans Block Bool
unreachableTrans _ [] = []
unreachableTrans (a:as) (b:bs) 
    | labelsIn  == [] = unreachableTrans as bs
    | otherwise = b : unreachableTrans as bs
    where
        labelsIn    = snd (snd a)

unreachable = fixpoint (helper makeCfg union unreachableGen unreachableKill unreachableTrans Forwards)

