module KleeneAlgebra
( CommandDom(..)
, zero
, one
, choice
, compose
, star
) where

import PauliOperator
import StabilizerTableau

newtype CommandDom = CommandDom StabTableau deriving (Eq, Show)

-----------------------------
-- Constans
-----------------------------
zero :: Int -> CommandDom
zero n = CommandDom (infeasibleTableau (2 * n))

one :: Int -> CommandDom
one n = CommandDom (Tableau (2 * n) gens)
    where
        gens = [tensorProduct (xPauli n i) (xPauli n i) | i <- [0 .. n - 1]] ++
               [tensorProduct (zPauli n i) (zPauli n i) | i <- [0 .. n - 1]]

-----------------------------
-- Operations
-----------------------------
choice :: CommandDom -> CommandDom -> CommandDom
choice = undefined

compose :: CommandDom -> CommandDom -> CommandDom
compose = undefined

star :: CommandDom -> CommandDom
star = undefined