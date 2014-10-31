module Assembly where

import Data.List

type Program = [Function]

data Function = Function Assembly.Id [Assembly.Id] [Block]
                deriving (Show, Eq)

data Block = Block Assembly.Num [Instruction]
             deriving (Show, Eq)

data Instruction = Lc Assembly.Reg Assembly.Num
                 | Ld Assembly.Reg Assembly.Id
                 | St Assembly.Id Assembly.Reg
                 | Add Assembly.Reg Assembly.Reg Assembly.Reg
                 | Sub Assembly.Reg Assembly.Reg Assembly.Reg
                 | Mul Assembly.Reg Assembly.Reg Assembly.Reg
                 | Div Assembly.Reg Assembly.Reg Assembly.Reg
                 | Lt Assembly.Reg Assembly.Reg Assembly.Reg
                 | Gt Assembly.Reg Assembly.Reg Assembly.Reg
                 | Eq Assembly.Reg Assembly.Reg Assembly.Reg
                 | Br Assembly.Reg Assembly.Num Assembly.Num
                 | Ret Assembly.Reg
                 | Call Assembly.Reg Assembly.Id [Assembly.Reg]
                 deriving (Eq)

-- For the re-assembly of the program into valid intermediate code we need
-- to add some pretty-printing instances for the Instruction type.
instance Show Instruction where
    show (Lc a b)   = "lc " ++  "r" ++ show  a ++ " "  ++ show b
    show (Ld a b)   = "ld " ++  "r" ++ show  a ++ " "  ++ b
    show (St a b)   = "st " ++  a ++ " "  ++ "r" ++ show b
    show (Add a b c)   = "add " ++  "r" ++ show  a ++ " " ++ "r" ++ show b ++ " " ++ "r" ++ show c
    show (Sub a b c)   = "sub " ++  "r" ++ show  a ++ " " ++ "r" ++ show b ++ " " ++ "r" ++ show c
    show (Mul a b c)   = "mul " ++  "r" ++ show  a ++ " " ++ "r" ++ show b ++ " " ++ "r" ++ show c
    show (Div a b c)   = "div " ++  "r" ++ show  a ++ " " ++ "r" ++ show b ++ " " ++ "r" ++ show c
    show (Lt a b c)   = "lt " ++  "r" ++ show  a ++ " " ++ "r" ++ show b ++ " " ++ "r" ++ show c
    show (Gt a b c)   = "gt " ++  "r" ++ show  a ++ " " ++ "r" ++ show b ++ " " ++ "r" ++ show c
    show (Eq a b c)   = "eq " ++  "r" ++ show  a ++ " " ++ "r" ++ show b ++ " " ++ "r" ++ show c
    show (Br a b c)   = "br " ++  "r" ++ show  a ++ " " ++ show b ++ " " ++ show c
    show (Ret a)    = "ret " ++ "r" ++ show a
    show (Call a b c)   = "call " ++  "r" ++ show  a ++ " " ++ b ++ " " ++ intercalate " " (map (("r" ++) . show) c)
   
type Num = Integer

type Reg = Integer

type Id = String

-- Get all the blocks in a function
getBlocks :: Function -> [Block]
getBlocks (Function _ _ bs) = bs

-- Find a block with a particular block id
lookupBlock :: [Block] -> Assembly.Num -> Block
lookupBlock [] _ = error "block doesn't exist"
lookupBlock (b@(Block id _):bs) n = if id == n then b else lookupBlock bs n
