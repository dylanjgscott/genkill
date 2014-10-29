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

main :: IO ()
main = do
    fileContents <- readFile "../tests/input/deadcode2.txt"
    
    let prog = parse . alexScanTokens $ fileContents

    -- Print the graphs for a whole program
    print . show . deadcode . getBlocks $ prog!!0
