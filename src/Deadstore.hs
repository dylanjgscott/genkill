module Deadstore where

import Data.List
import Data.Maybe

import Cfg
import Genkill
import Assembly
import Util

use :: Gen Block Id
use (Block _ is) = foldl union [] (map usedVars is)

def :: Kill Block Id
def (Block _ is) = foldl union [] (map definedVars is)

usedVars :: Instruction -> [Id]
usedVars reg = case reg of
    (Ld _ x) -> [x]
    otherwise -> []

definedVars :: Instruction -> [Id]
definedVars reg = case reg of
    (St x _) -> [x]
    otherwise -> []

deadstoreTrans :: Transform Block Block Id
deadstoreTrans _ [] = []
deadstoreTrans flowdata (b:bs) = deleteDeadstore b usedVars : deadstoreTrans flowdata bs
    where
    usedVars = fst . fromJust $ lookup b flowdata

deleteDeadstore :: Block -> [Id] -> Block
deleteDeadstore (Block num ins) liveVars = Block num liveIns
    where
    liveIns :: [Instruction]
    liveIns = filter isAlive ins
    isAlive :: Instruction -> Bool
    isAlive i = (definedVars i) \\ liveVars == []
        
deadstoreBlockTransform :: [Block] -> [Block]
deadstoreBlockTransform = fixpoint (runGenKill makeBlockCfg union use def deadstoreTrans Backwards)

deadstore :: Program -> Program
deadstore p = applyBlockTransform deadstoreBlockTransform p
