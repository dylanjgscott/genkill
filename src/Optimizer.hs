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

optArg :: String -> [String] -> Bool
optArg opt args = opt `elem` init args

main :: IO ()
main = do
    args <- getArgs

    let filename = last args

    fileContents <- readFile filename
    
    let prog = parse . alexScanTokens $ fileContents

  
--    let opts = concat [ [ "unreachable" | (optArg "-u" args) ],
--                        [ "Dead Code"   | (optArg "-d" args) ],
--                        [ "Loads"       | (optArg "-l" args) ]]
--    
--    if opts == [] then
--            print "do all..." 
--        else
--            print "one select"
    
    
    
    if optArg "-u" args then 
             print . show . unreachable . getBlocks $ prog!!0
       else print "Leaving unreachable code."
    if optArg "-l" args then 
             print . show . unreachable . getBlocks $ prog!!0
       else print "Not Removing deadcode."
    if optArg "-l" args then 
             print . show . unreachable . getBlocks $ prog!!0
       else print "Not optimising loads."



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
--
opts args
    | ut == False && dt == False && lt == False = getOpts True True True
    | otherwise = getOpts ut dt lt
    where
        ut = optArg "-u" args
        dt = optArg "-d" args
        lt = optArg "-l" args


getOpts u d l =  concat [   [ "unreachable" | u ],
                            [ "Dead Code"   | d ],
                            [ "Loads"       | l ]]
    

