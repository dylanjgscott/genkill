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
-- Block zero generate a label "True".
-- As the Gen/Kill algorithm traverses the graph of blocks this label with 
-- flow to all blocks connected to block zero.
-- No block can kill the connection of a predecessor to block zero.
-- We can then check the labels in produced by Gen/Kill. 
-- An empty list (No true value) indicates no connection to block zero
-- and therefore unreachable block has been found and can be eliminated.
-- Any block with True values in their labels must be connected to block zero
-- and therefore is left in the graph.
---------------------------------------------------------------------------


-------------------------------
-- Gen Function.
-- If encountered block is block zero it generate the label "True"
-- Othwise no labels are generated.
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
-- Check provded Block labels tuple 
-- for True values - ie connection to block zero.
-- Delete if not.
unreachableTrans :: Transform Block Block Bool
unreachableTrans _ [] = []
unreachableTrans (a:as) (b:bs) 
    | labelsIn  == [] = unreachableTrans as bs
    | otherwise = b : unreachableTrans as bs
    where
        labelsIn    = snd (snd a)
------------------------------
-- helper function.
-- Packages up needed parameters and passes everything to
-- the fixpoint function
unreachableBlockTransform = 
    fixpoint (runGenKill makeBlockCfg union unreachableGen unreachableKill unreachableTrans Forwards)

------------------------------
-- Wrapper for wrapper. 
-- Takes and return the program and
-- Will map the genkill function
-- onto the entire program.
unreachable :: Program -> Program 
unreachable p = applyBlockTransform unreachableBlockTransform p
