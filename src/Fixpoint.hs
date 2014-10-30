{-# LANGUAGE ScopedTypeVariables #-}

module Fixpoint where

import Cfg
import Genkill
import Assembly
import Data.List

-- | Types for Pretty Program 
--type ExeProgram = [Function]
--type ExeFunction = (Id, [Id], [Block])
--type ExeBlock = Block
--type ExeInstruction = [String]
--type ExeId = String
--type ExeBlockId = Int
--type ExeRegister = Int





type Trans a b = GenKillOut a b -> [a] -> [a]

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x
    | fx == x = x
    | otherwise = fixpoint f (fx)
    where fx = f x

applyTransformation :: forall a b . (Eq a, Eq b)
       => MakeCfg a
       -> Meet b
       -> Gen a b
       -> Kill a b
       -> Trans a b
       -> Direction
       -> [a]
       -> [a]
applyTransformation makeCfg meet gen kill trans direction x
    = trans (genkill cfg meet gen kill direction x) x
    where
        cfg = makeCfg x

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
