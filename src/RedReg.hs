module RedReg where 

import Data.List

import Cfg
import Genkill
import Assembly
import Fixpoint

-- [(val, reg1)]

-- [(reg2, reg1)]

-- reg2 is eliminated

makeCFG

gen :: Gen Instruction [(Istruction, Integer)]

kill :: Gen 

redreg = fixpoint (runGenKill )