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
    :: forall b c . (Eq b, Eq c, Show c, Show b)
    => Cfg b
    -> Meet c
    -> Gen b c
    -> Kill b c
    -> Direction
    -> Labels b c
genkill (Cfg ns es) meet gen kill direction = [(x, (labelin n, labelout n)) | n@(CfgNode x) <- ns]

    where

    -- This list different returns the first list, where every element
    -- does not occur in the second list
    diff [] _ = []
    diff (x:xs) ys =
      if elem x ys 
      then diff xs ys
      else x:(diff xs ys)
    -- Transfer function.
    trans :: CfgNode b -> [c] -> [c]
    trans node@(CfgNode z) x = (gen z) `union` (diff (nub x) (kill z))

    -- Generates all the labels coming in to a node.
    labelin :: CfgNode b -> [c]
    labelin node
        | direction == Backwards = trans node (labelout node)
        | direction == Forwards = foldl meet first preds
        where
            first = if not (null preds) then preds !! 0 else []
            preds = [labelout p | p <- Cfg.pred es node]
        

    -- Generates all the labels coming out of a node.
    labelout :: CfgNode b -> [c]
    labelout node
        | direction == Forwards = trans node (labelin node)
        | direction == Backwards = foldl meet first succ
        where
            first = if not (null succ) then succ !! 0 else []
            succ  = [labelin s | s <- Cfg.succ es node]

runGenKill :: forall a b c . (Eq b, Eq c, Show c, Show b)
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
