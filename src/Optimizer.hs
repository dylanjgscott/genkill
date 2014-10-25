-- Main source file for Optimizer
-- Calls all other Modules

import System.Environment
import System.Exit
import System.Console.GetOpt
import Data.Char
import Data.List





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



    print $ show filename
    print $ show options
