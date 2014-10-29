{-# LANGUAGE ScopedTypeVariables #-}

module Genkill where

import Data.List

import Cfg

data Direction = Forwards | Backwards
               deriving Eq

type MakeCfg a = [a] -> Cfg a
type Meet b = [b] -> [b] -> [b]
type Gen a b = a -> [b]
type Kill a b = a -> [b]
type GenKillOut a b = [(a, ([b], [b]))]

-- Generate all the in and out labels for all the nodes in a graph.
genkill
    :: forall a b . (Eq a, Eq b)
    => Cfg a
    -> Meet b
    -> Gen a b
    -> Kill a b
    -> Direction
    -> [a]
    -> GenKillOut a b
genkill cfg@(Cfg ns es) meet gen kill direction xs = [(x, (labelin n, labelout n)) | n@(CfgNode x) <- ns]

    where

    -- Transfer function.
    trans :: CfgNode a -> [b] -> [b]
    trans node@(CfgNode z) x = (gen z) `union` ((nub x) \\ (kill z))

    -- Generates all the labels coming in to a node.
    labelin :: CfgNode a -> [b]
    labelin node
        | direction == Forwards = foldl meet [] [labelout p | p <- Cfg.pred es node]
        | direction == Backwards = trans node (labelout node)

    -- Generates all the labels coming out of a node.
    labelout :: CfgNode a -> [b]
    labelout node@(CfgNode x)
        | direction == Forwards = trans node (labelin node)
        | direction == Backwards = foldl meet [] [labelin s | s <- Cfg.succ es node]

