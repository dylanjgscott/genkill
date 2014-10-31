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
import Deadstore
import Unreachable
import RedReg
import Util
import Cfg


noop :: Program -> Program
noop p = p

-- apply every function in a list to an entity
-- with the output of one function being the input
-- of the next
frobinate :: [a -> a] -> a -> a
frobinate [] x = x
frobinate (f:fs) x = frobinate fs (f x)

-- Generate a list of optimization functions based on
-- command line options.
optimisationOptions :: [(String, Program -> Program)]
optimisationOptions = [
        ("-u", unreachable),
        ("-d", deadcode . deadstore),
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

    -- Parse the assembly program into an AST using alex and happy
    let program = parse . alexScanTokens $ fileContents

    -- identify which optimizations to perform 
    let optimisations = parseOptions options

    -- perform fixpoint over all the optimizations
    -- arguably not needed but removed the need to ensure that
    -- redundant load is performed before dead code for example
    let optimizer = fixpoint (frobinate optimisations)

    -- output the optimized program
    putStr . showProgram $ optimizer program
