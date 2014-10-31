module Util where

import Assembly
import Data.List


-- Perform fixed point operation on an entity given a valid
-- transform function
fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x
    | fx == x = x
    | otherwise = fixpoint f (fx)
    where fx = f x

-- Wrap a program such that an optimizations is applied to every function
applyBlockTransform :: ([Block] -> [Block]) -> Program -> Program
applyBlockTransform _ [] = []
applyBlockTransform t ((Function id args blocks):fs) =
    Function id args (t blocks) : applyBlockTransform t fs


-- Generate the (Block number, Line number) to Instruction pairs needed
-- for an instruction based GenKill graph (so that the transformation)
-- is reversible
mapBlockToInstr :: Integer -> [Instruction] -> [((Integer, Integer), Instruction)]
mapBlockToInstr blk instructs = (zip (zip [blk,blk..] [0..]) instructs)


-- Pretty printing functions, simple traversal of data structure
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
