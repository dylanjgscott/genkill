{-# LANGUAGE ScopedTypeVariables #-}

module Util where

import Assembly

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x
    | fx == x = x
    | otherwise = fixpoint f (fx)
    where fx = f x

applyBlockTransform :: ([Block] -> [Block]) -> Program -> Program
applyBlockTransform _ [] = []
applyBlockTransform t ((Function id args blocks):fs) =
    Function id args (t blocks) : applyBlockTransform t fs
