-- Main source file for Optimizer
-- Calls all other Modules

import System.Environment
import System.Exit
import System.Console.GetOpt
import Data.Char
import Data.List
import Data.Maybe

import Lexer
import Parser
import Assembly
import Deadcode
import Unreachable
import RedReg
import Util

noop :: Program -> Program
noop p = p

frobinate :: [a -> a] -> a -> a
frobinate [] x = x
frobinate (f:fs) x = frobinate fs (f x)

optimisationOptions :: [(String, Program -> Program)]
optimisationOptions = [
        ("-u", unreachable),
        ("-d", deadcode),
        ("-l", redreg)
    ]

parseOptions :: [String] -> [Program -> Program]
parseOptions = map (\x -> fromJust (lookup x optimisationOptions))

main :: IO ()
main = do

    args <- getArgs

    let options = init args
    let filename = last args

    fileContents <- readFile filename
    let program = parse . alexScanTokens $ fileContents
    --putStr (show (makeCfg (getBlocks (program !! 0))))
    let optimisations = parseOptions options

    let optimizer = fixpoint (frobinate optimisations)

    putStr . showProgram $ optimizer program
