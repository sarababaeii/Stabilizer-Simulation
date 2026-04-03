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

import Data.Bits (xor)

-- Note: It's always on 2n qubits, where the first n qubits are "input" and the last n qubits are "output".
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

compose :: CommandDom -> CommandDom -> CommandDom
compose (CommandDom (Tableau n1 gen1)) (CommandDom (Tableau n2 gen2)) 
    | n1 /= n2  = error ("Tableaux must have the same number of qubits for composition: " ++ show n1 ++ " vs " ++ show n2)
    | otherwise = CommandDom (projectOutBy (composeProjMap n) (BiTableau (n, n1) (zip leftGens rightGens)))
    where
        n = n1 `div` 2
        id_n = identityPauli n
        (gen1In, gen1Out) = unzip $ map splitInOutPauli gen1
        (gen2In, gen2Out) = unzip $ map (applyTransposedPhase . splitInOutPauli) gen2
        leftGens = gen1Out ++ gen2In
        rightGens = map (`tensorProduct` id_n) gen1In ++ map (tensorProduct id_n) gen2Out

applyTransposedPhase :: (Pauli, Pauli) -> (Pauli, Pauli)
applyTransposedPhase (pIn, (Pauli xs zs c d)) = (pIn, Pauli xs zs (c `xor` transposedPhase pIn) d)

composeProjMap :: Int -> (Pauli, Pauli) -> Maybe Pauli
composeProjMap n (p1, p2@(Pauli xs zs _ _))
    | isLikeIdentity p1 = Just (Pauli xs zs c' d')
    | otherwise         = Nothing
    where 
        (c', d') = phaseAfterGroupOp n p1 p2

star :: CommandDom -> CommandDom
star = undefined

-- Test
sGate :: CommandDom
sGate = CommandDom (sTab)
zGate :: CommandDom
zGate = CommandDom (zTab)

g :: CommandDom
g = CommandDom (gTab)
h :: CommandDom
h = CommandDom (hTab)