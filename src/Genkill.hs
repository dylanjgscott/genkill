{-# LANGUAGE ScopedTypeVariables #-}

module Genkill where

import Data.List

import Cfg

data Direction = Forwards | Backwards
               deriving Eq

type Meet c = [c] -> [c] -> [c]
type Gen b c = b -> [c]
type Kill b c = b -> [c]
type Labels b c = [(b, ([c], [c]))]
type Transform a b c = Labels b c -> [a] -> [a]

-- Generate all the in and out labels for all the nodes in a graph.
genkill
    :: forall b c . (Eq b, Eq c)
    => Cfg b
    -> Meet c
    -> Gen b c
    -> Kill b c
    -> Direction
    -> Labels b c
genkill (Cfg ns es) meet gen kill direction = [(x, (labelin n, labelout n)) | n@(CfgNode x) <- ns]

    where

    -- Transfer function.
    trans :: CfgNode b -> [c] -> [c]
    trans node@(CfgNode z) x = (gen z) `union` ((nub x) \\ (kill z))

    -- Generates all the labels coming in to a node.
    labelin :: CfgNode b -> [c]
    labelin node
        | direction == Forwards = foldl meet [] [labelout p | p <- Cfg.pred es node]
        | direction == Backwards = trans node (labelout node)

    -- Generates all the labels coming out of a node.
    labelout :: CfgNode b -> [c]
    labelout node@(CfgNode x)
        | direction == Forwards = trans node (labelin node)
        | direction == Backwards = foldl meet [] [labelin s | s <- Cfg.succ es node]

runGenKill :: forall a b c . (Eq b, Eq c)
       => MakeCfg a b
       -> Meet c
       -> Gen b c
       -> Kill b c
       -> Transform a b c
       -> Direction
       -> [a]
       -> [a]
runGenKill makeCfg meet gen kill trans direction x
    = trans (genkill cfg meet gen kill direction) x
    where
        cfg = makeCfg x
