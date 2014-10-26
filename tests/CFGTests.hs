module CFGTests where

import TestLib
import CFG

import Test.HUnit
import Control.Exception
import Control.Monad



-- | Tests

test0 = TestCase (do
            assertEqual "Placeholder Test True == True." True True)







cFGTests = TestList [TestLabel "test0" test0]


