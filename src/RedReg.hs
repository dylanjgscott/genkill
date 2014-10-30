module RedReg where 

import Data.List
import Data.Maybe

import Cfg
import Genkill
import Assembly
import Util

data LoadLabel = LoadLabel (Reg, Id)


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

redregTrans _ [] = []
redregTrans _ inp = inp

redreg = applyBlockTransform (fixpoint (runGenKill makeInstrCfg intersect gen kill redregTrans Forwards))

-- loosely compared tuple

-- (a,b) == (a,c)
-- (a,b) == (c,b)

--gen :: Gen Instruction [(Istruction, Integer)]

--kill :: Gen Instruction

--redreg = fixpoint (runGenKill )

---- makeCfg meet gen kill trans direction x