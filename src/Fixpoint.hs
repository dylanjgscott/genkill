{-# LANGUAGE ScopedTypeVariables #-}

module Fixpoint where

import Cfg
import Genkill
import Assembly

type Trans a b = GenKillOut a b -> [a] -> [a]

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x
    | fx == x = x
    | otherwise = fixpoint f (fx)
    where fx = f x

applyTransformation :: forall a b . (Eq a, Eq b)
       => MakeCfg a
       -> Meet b
       -> Gen a b
       -> Kill a b
       -> Trans a b
       -> Direction
       -> [a]
       -> [a]
applyTransformation makeCfg meet gen kill trans direction x
    = trans (genkill cfg meet gen kill direction x) x
    where
        cfg = makeCfg x

applyBlockTransform :: ([Block] -> [Block]) -> Program -> Program
applyBlockTransform _ [] = []
applyBlockTransform t ((Function id args blocks):fs) =
    Function id args (t blocks) : applyBlockTransform t fs

