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
zero n = CommandDom (unsatTableau (2 * n))

one :: Int -> CommandDom
one n = CommandDom (Tableau (2 * n) gens)
    where
        gens = [tensorProduct (xPauli n i) (xPauli n i) | i <- [0 .. n - 1]] ++
               [tensorProduct (zPauli n i) (zPauli n i) | i <- [0 .. n - 1]]

-----------------------------
-- Operations
-----------------------------
choice :: CommandDom -> CommandDom -> CommandDom
choice (CommandDom tab1) (CommandDom tab2) = CommandDom (intersection tab1 tab2)

-- compose :: CommandDom -> CommandDom -> CommandDom
-- compose (CommandDom (StabTableau n1 gen1)) (CommandDom (StabTableau n2 gen2)) 
--     | n1 /= n2  = error ("Tableaux must have the same number of qubits for composition: " ++ show n1 ++ " vs " ++ show n2)
--     | otherwise = CommandDom (projectOut (BiTableau (??) (zip leftGens rightGens)))
--     where
--         gen1' = map splitInOutPauli gen1
--         gen1In = map fst gen1'
--         gen1Out = map snd gen1'
--         gen2' = map splitInOutPauli gen2
--         gen2In = map fst gen2'
--         gen2Out = map snd gen2'
--         leftGens = gen1Out ++ gen2In
--         rightGens = map (\g -> tensorProduct g (identityPauli n1)) gen1In ++ map (tensorProduct (identityPauli n1)) gen2Out


star :: CommandDom -> CommandDom
star = undefined