{-# LANGUAGE ScopedTypeVariables #-}

module Genkill where

import Data.List

import Cfg

type MakeCfg a = [a] -> Cfg a
type Comb b = [b] -> [b] -> [b]
type Gen a b = a -> [b]
type Kill a b = a -> [b]
type GenKillOut a b = [(a, ([b], [b]))]

-- Generate all the in and out labels for all the nodes in a graph.
genkill
    :: forall a b . (Eq a, Eq b)
    => Cfg a
    -> Comb b
    -> Gen a b
    -> Kill a b
    -> [a]
    -> GenKillOut a b
genkill cfg@(Cfg ns es) comb gen kill xs = [(x, (labelin n, labelout n)) | n@(CfgNode x) <- ns]

    where

    -- Generates all the labels coming in to a node.
    labelin :: CfgNode a -> [b]
    labelin n = foldl comb [] [labelout p | p <- pred es n]

    -- Generates all the labels coming out out of a node.
    labelout :: CfgNode a -> [b]
    labelout n@(CfgNode x) = comb (gen x) ((nub (labelin n)) \\ (kill x))

    -- Lists all the nodes with an edge ending at the given node.
    pred :: [CfgEdge a] -> CfgNode a -> [CfgNode a]
    pred [] _ = []
    pred ((CfgEdge (src, dst)):es) n
        | n == dst = src : pred es n
        | otherwise = pred es n
