module CFG where

-- | Other Libraries
import Data.List

-- | Package libraires
import Assembly



-- | Data Types for the Graph


-- Node is a label refering to it's parent function and the corresponding block
data Node = Node {  func    :: String,
                    basic   :: Block
                 }  deriving (Show, Eq)

-- Edges are "from" a node "to" node
data Edge = Edge {  from    :: Node,
                    to      :: Node
                 }  deriving (Show, Eq)

-- Graph is a List of Nodes and a List of Edges
data Graph = Graph { nodes  :: [Node],
                     edges  :: [Edge]
                   } deriving (Show, Eq) -- I think we need Eq here for fixed pt

-- Type for Node labels [(func Node) (Num Block)]
type Label = (String, Integer)


-- | Node level helper functions

-- Get the Block number of a Node
getNodeBlockNumber                          :: Node -> Integer
getNodeBlockNumber (Node {basic = block})   = getBlockNumber block

-- Get the Number of a block
-- If Block definition had field names could probably ditch this.
getBlockNumber          :: Block -> Integer
getBlockNumber (Block num _ ) = num

-- Get Block instructions
getBlockInsts                :: Block -> [Instruction]
getBlockInsts (Block _ inst) = inst

-- | Edge Level Helper Functions














-- | Graph Level Helper Functions










-- | The main game. The CFG functions themselves 

-- Entry point to the module. Takes the provided graph and returns a graph
buildMeAGraph          ::  Program -> Graph
buildMeAGraph (f:fs)   = undefined

-- First pass of program. Build list of nodes as they are
buildListOfBasicBlocks        :: Program -> [Node]
buildListOfBasicBlocks (f:fs) = undefined


-- Second pass. Take List of Nodes and split at function calls
buildListOfSplitBasicBlocks          :: [Node] -> [Node]
buildListOfSplitBasicBlocks (x:xs)   = undefined

-- Take a label and split that node into two.
splitNode   ::  Node -> [Node]
splitNode   = undefined

-- update block numbers. Take a node to start from and increment
-- following basic Block positions
-- Takes a Start Node, The Node List and returns a new List of Nodes
-- Is it worth doing each function's nodes separately and joining at end?
updateBasicBlockNums            :: Node -> [Node] -> [Node] 
updateBasicBlockNums node (n:ns) = undefined
-- Gosh - need to fin every node that matchs 'func' field and has
-- num >= node num and increment by one

-- Increment position of a selected basic block
incrBlockPos                                     :: Node -> Node
incrBlockPos (Node {func = name, basic = block}) = 
                Node name (Block ((getBlockNumber block) + 1)(getBlockInsts block))



-- Third pass. Take split blocks and generate edges - therefore - Graph
buildEdgesAndGraph          :: [Node] -> Graph
buildEdgesAndGraph (x:xs)   = undefined






