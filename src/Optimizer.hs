-- Main source file for Optimizer
-- Calls all other Modules

import System.Environment
import System.Exit
import System.Console.GetOpt
import Data.Char






options :: [OptDescr a]
options = []



main :: IO ()
main = do
    [filename, options]    <- getArgs
    print $ show filename
    print $ show options
