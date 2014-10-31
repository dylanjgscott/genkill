module Unreachable where

import Data.List
-- | Other Needed Assignment 3 Modules
import Cfg
import Genkill
import Assembly
import Util


---------------------------------------------------------------------------
-- | Genkill parameters.
--
-- Our generalised Genkill framwork requires definitions for the functions
-- Gen
-- Kill
-- Graph construction
-- Parameter for list meeting: union/interset etc
-- Transformation function
-- Flow direction
--
--
-- Unreachable Code.
--
-- Basic Idea:
-- Block zero generatea label "True".
-- As the Gen/Kill algorithm traverses the graph of blocks this label will 
-- flow to all descendant blocks of block zero.
-- No block can kill the connection of a predecessor to block zero.
-- We can then check the labels produced by Gen/Kill. 
-- An empty list (No true value) indicates no connection to block zero
-- and therefore an unreachable block has been found and can be eliminated.
-- Any block with True values in their labels must be connected to block zero
-- and therefore is left in the graph.
---------------------------------------------------------------------------


-------------------------------
-- Gen Function.
-- As the entry point for a given function block zero generates the label "True"
-- Otherwise no labels are generated.
unreachableGen :: Gen Block Bool
unreachableGen (Block num _) 
    | num == 0 = [True]
    | otherwise = []

-------------------------------
-- Kill function
-- For unreachable code no Block
-- Can kill the connetion of a previous block
-- to block zero.
unreachableKill :: Kill Block Bool
unreachableKill _ = []

------------------------------
-- Transform function
-- Check provided Block labels tuple 
-- True values - ie connection to block zero - are returned.
-- If empty list indicates no connection to block zero and we disregard the block
unreachableTrans :: Transform Block Block Bool
unreachableTrans _ [] = []
unreachableTrans (a:as) (b:bs) 
    | labels  == [] = unreachableTrans as bs
    | otherwise = b : unreachableTrans as bs
    where
        labels    = snd (snd a)
------------------------------
-- helper function.
-- Packages up module provided parameters and passes everything to
-- the fixpoint function
-- "Forwards" parameter indicates to the genkill framwork which direction it needs to 
-- traverse the graph. Unreachable code is a forward looking flow analysis problem. 
-- "makeBlockCfg" passes the block level graphing function defined in the Cfg Module.
-- "union" indicates the meet function our genkill framework should use. In this case a union
-- of gen and kill lists as we want all possible paths to a given block to be considered.
unreachableBlockTransform = 
    fixpoint (runGenKill makeBlockCfg union unreachableGen unreachableKill unreachableTrans Forwards)

------------------------------
-- Wrapper for wrapper. 
-- Takes and returns the program and
-- Will map the genkill function
-- onto the entire program function by function.
unreachable :: Program -> Program 
unreachable p = applyBlockTransform unreachableBlockTransform p
