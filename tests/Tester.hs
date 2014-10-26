
-- | Tests
import CFGTests


-- | Dependencies 
import Test.HUnit





main = do
    putStrLn("Running some optimiser tests.")
    putStrLn("-----------------------------")

    putStrLn("Control Flow Graph Tests: ")
    runTestTT cFGTests


