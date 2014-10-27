-- Main source file for Optimizer
-- Calls all other Modules

import System.Environment
import System.Exit
import System.Console.GetOpt
import Data.Char
import Data.List

import Lexer
import Parser
import Assembly
import Genkill

options :: [OptDescr a]
options = []

main :: IO ()
main = do
    [filename, options]    <- getArgs

    if (isInfixOf "u" options)
        then print "Remove unreachable code."   -- Run U
        else print "Leaving unreachable code."

    if (isInfixOf "d" options)
        then print "Remove dead code."          -- Run dead
        else print "Leaving dead loads."


    if (isInfixOf "l" options)                  -- Run l
        then print "Remove redundant loads."
        else print "Leaving redundant loads."

    fileContents <- readFile filename

    print . show . makeGraphs . parse . alexScanTokens $ fileContents

getBlocks :: Function -> [Block]
getBlocks (Function _ _ bs) = bs

makeGraph :: [Block] -> Graph Block
makeGraph bs = Graph nodes edges
    where
    nodes :: [Node Block]
    nodes = map Node bs
    edges :: [Edge Block]
    edges = concat $ map blockEdges bs

    blockEdges :: Block -> [Edge Block]
    blockEdges b@(Block _ is) = map (\x -> Edge (Node b, Node x)) (blockLinks is)

    blockLinks :: [Instruction] -> [Block]
    blockLinks [] = []
    blockLinks (Br _ id1 id2:is) = getBlock bs id1 : getBlock bs id2 : blockLinks is
    blockLinks (_:is) = blockLinks is

makeGraphs :: Program -> [Graph Block]
makeGraphs = map (makeGraph . getBlocks)

getBlock :: [Block] -> Assembly.Num -> Block
getBlock [] _ = error "block doesn't exist"
getBlock (b@(Block id _):bs) n = if id == n then b else getBlock bs n
