module Assembly where

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
                 deriving (Show, Eq)

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
