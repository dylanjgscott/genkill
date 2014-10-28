{-# LANGUAGE ScopedTypeVariables #-}

module Fixpoint where

import Cfg
import Genkill

type Trans a b = GenKillOut a b -> [a] -> [a]

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x
    | fx == x = x
    | otherwise = fixpoint f (fx)
    where fx = f x

helper :: forall a b . (Eq a, Eq b)
       => MakeCfg a
       -> Comb b
       -> Gen a b
       -> Kill a b
       -> Trans a b
       -> [a]
       -> [a]
helper makeCfg comb gen kill trans x = trans (genkill' x) x
    where
    genkill' = genkill cfg comb gen kill
    cfg = makeCfg x
