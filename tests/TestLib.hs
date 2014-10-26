{-# LANGUAGE CPP #-}
----------------------------------------------
-- Module for Helper functions common to all 
-- test files
--
---------------------------------------------

module TestLib where


-- Package files



--- Third party Libraries
import Test.HUnit
import Control.Exception
import Control.Monad


-- | Helper functions



-- Strip newlines from input file
filt input = filter (/= '\n') input




-- HUnit does not have an 'assertException or simmilar so need to make one
assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
assertException ex action =
    handleJust isWanted (const $return ()) $ do
        action
        assertFailure $ "Expected exception: " ++ show ex
    where isWanted = guard . (== ex)


#if __GLASGOW_HASKELL__ < 708

-- The following instance declaration is needed in < ghc 7.8 for our unit tests to be able to catch the error string.
-- However, in ghc 7.8 it has been included so I have resorted to preprocessor code in case this gets compiled in ghc 7.8+
-- Should now work on all ghc 7.4 .. 7.8

-- Need an instance decleration so I can pass a String in for the Error type
instance Eq ErrorCall where
    x == y = (show x) == (show y)


#endif
