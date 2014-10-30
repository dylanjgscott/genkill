module Util where

import Cfg
import Genkill
import Assembly
import Data.List


fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x
    | fx == x = x
    | otherwise = fixpoint f (fx)
    where fx = f x

applyBlockTransform :: ([Block] -> [Block]) -> Program -> Program
applyBlockTransform _ [] = []
applyBlockTransform t ((Function id args blocks):fs) =
    Function id args (t blocks) : applyBlockTransform t fs


-- Pretty printing functions, simple traversal of data structure
-- Fairly self explanatory
showProgram :: Program -> String
showProgram prog =  "( "
                 ++ intercalate "\n  " (map showFunction prog)
                 ++ " )\n"

showFunction :: Function -> String
showFunction (Function name args blocks) =  "("
                  ++ name
                  ++ " "
                  ++ showArgs args
                  ++ "\n    "
                  ++ showBlocks blocks
                  ++ " )"

showArgs :: [Id] -> String
showArgs args =  "("
              ++ intercalate " " args
              ++ ")"

showBlocks :: [Block] -> String
showBlocks blocks = intercalate "\n    " (map showBlock blocks)

showBlock :: Block -> String
showBlock (Block blkId instructs) =  "( "
                            ++ show blkId
                            ++ " "
                            ++ intercalate "\n        " (map showInstruct instructs)
                            ++ " )"

showInstruct        :: Instruction -> String
showInstruct instruct = "(" ++ show instruct ++ ")"

showReg :: Reg -> String
showReg reg = "r" ++ show reg
