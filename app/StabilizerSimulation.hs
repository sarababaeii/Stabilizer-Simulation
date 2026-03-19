{-# LANGUAGE DeriveGeneric #-}

module StabilizerSimulation
( Tableau(..)
, initialTableau
, applyGate'
, measure
, showTableau
) where

import Quantum (Gate(..), Measure(..), MeasureResult(..))
import PauliOperator

import Data.Maybe (isNothing)
import System.Random (StdGen)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

data Tableau = Tableau { qubitNum :: Int
                       , destabilizers :: [Pauli]
                       , stabilizers :: [Pauli]
                       } deriving (Generic, Eq, Show, Read)
instance NFData Tableau

initialTableau :: Int -> Tableau
initialTableau n = Tableau {qubitNum = n, destabilizers = destabs, stabilizers = stabs}
    where 
        destabs = [xPauli n i | i <- [0..(n - 1)]]
        stabs = [zPauli n i | i <- [0..(n - 1)]]

-------------
-- Operations
-------------
applyGate' :: Gate -> Tableau -> Tableau
applyGate' g Tableau {qubitNum = n, destabilizers = destabs, stabilizers = stabs} =
    let destabs' = map (\p -> applyGate g p) destabs
        stabs' = map (\p -> applyGate g p) stabs
    in Tableau {qubitNum = n, destabilizers = destabs', stabilizers = stabs'}

measure :: Measure -> Tableau -> StdGen -> ((Tableau, MeasureResult), StdGen)
measure M {qubit = a} t gen
    | isNothing pivot = ((t, determinateMeasure a t), gen)
    | otherwise       = randomMeasure (extractValue pivot) a t gen
    where 
        pivot = firstNonCompBasisPauli a (stabilizers t)

determinateMeasure :: Int -> Tableau -> MeasureResult
determinateMeasure a Tableau {qubitNum = n, destabilizers = destabs, stabilizers = stabs} =
    let id = identityPauli n
        ps = filterByFirst (\p -> not $ isInCompBasis a p) destabs stabs
        sump = foldl (\acc p -> (groupOp p acc)) id ps
    in MR {qubit' = a, result = phaseBool sump, isRandom = False}

randomMeasure :: (Pauli, Int) -> Int -> Tableau -> StdGen -> ((Tableau, MeasureResult), StdGen)
randomMeasure pivot a Tableau {qubitNum = n, destabilizers = destabs, stabilizers = stabs} gen =
    let (destabs', _) = rowsAfterRandomMeas False pivot 0 a destabs gen
        (stabs', gen') = rowsAfterRandomMeas True pivot 0 a stabs gen
        pInd = snd pivot
        res = phaseBool (stabs' !! pInd)
    in ((Tableau {qubitNum = n, destabilizers = destabs', stabilizers = stabs'}, 
            MR {qubit' = a, result = res, isRandom = True}), 
            gen')

-------------------
-- Updating Tableau
-------------------
rowsAfterRandomMeas :: Bool -> (Pauli, Int) -> Int -> Int -> [Pauli] -> StdGen -> ([Pauli], StdGen)
rowsAfterRandomMeas _ _ _ _ [] gen = ([], gen)
rowsAfterRandomMeas isStabs (p, pInd) ind a (x:xs) gen
    | ind == pInd = 
        let (p', gen') = (pivotAfterRandomMeas isStabs p a gen)
            (rest, gen'') = rowsAfterRandomMeas isStabs (p, pInd) (ind + 1) a xs gen'
        in ((p':rest), gen'')
    | otherwise   = 
        let (rest, gen') = rowsAfterRandomMeas isStabs (p, pInd) (ind + 1) a xs gen
        in (((nonPivotsAfterRandomMeas p x a) : rest), gen')

nonPivotsAfterRandomMeas :: Pauli -> Pauli -> Int -> Pauli
nonPivotsAfterRandomMeas p q a
    | isInCompBasis a q = q
    | otherwise         = groupOp p q

pivotAfterRandomMeas :: Bool -> Pauli -> Int -> StdGen -> (Pauli, StdGen)
pivotAfterRandomMeas False p _ gen = (p, gen) -- Destabilizer
pivotAfterRandomMeas True p a gen = measuringPauli p a gen

----------------------
-- Auxiliary functions
----------------------
filterByFirst :: (a -> Bool) -> [a] -> [b] -> [b]
filterByFirst _ [] _ = []
filterByFirst _ _ [] = []
filterByFirst p (x:xs) (y:ys)
    | p x       = y:rest
    | otherwise = rest
    where
        rest = filterByFirst p xs ys

extractValue :: Maybe a -> a
extractValue (Just x) = x

-- Testing
showTableau :: Tableau -> String
showTableau Tableau {qubitNum = _, destabilizers = destabs, stabilizers = stabs} =
    showPaulies destabs ++ "---------------" ++ showPaulies stabs