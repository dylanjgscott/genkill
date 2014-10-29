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
import Deadcode
import Unreachable

main :: IO ()
main = do
    args <- getArgs

    let filename = last args

    fileContents <- readFile filename
    
    let prog = parse . alexScanTokens $ fileContents


    if "-u" `elem` args then 
             --print "Remove unreachable code."  -- Run U
             
             -- Print the graphs for a whole program
             
             print . show . unreachable . getBlocks $ prog!!0
        
        else print "Leaving unreachable code."

--    if (isInfixOf "d" options)
--        then print "Remove dead code."          -- Run dead
--        else print "Leaving dead loads."
--
--
--    if (isInfixOf "l" options)                  -- Run l
--        then print "Remove redundant loads."
--        else print "Leaving redundant loads."
--
    

    -- Print the graphs for a whole program
--    print . show . deadcode . getBlocks $ prog!!0
