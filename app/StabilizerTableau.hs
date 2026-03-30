{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module StabilizerTableau
( StabTableau(..)
, infeasibleTableau
) where

import PauliOperator
import Utils (extractValue)

import Data.Maybe (isNothing)
import Data.List (takeWhile)
import Data.Bit

data StabTableau = Tableau { qubitNum :: Int
                           , generators :: [Pauli]
                           } deriving (Eq, Read)

instance Show StabTableau where
    show :: StabTableau -> String
    show (Tableau _ gens) =
        unlines $ map show gens

data BiTableau = BiTableau { qubitNums :: (Int, Int)
                           , biGenerators :: [(Pauli, Pauli)]
                           } deriving (Eq, Read)

instance Show BiTableau where
    show :: BiTableau -> String
    show (BiTableau  _ paulies) =
        unlines $ map (\(p1, p2) -> show p1 ++ " || " ++ show p2) paulies

---------------------------------------
-- Auxiliary types
---------------------------------------
class TableauRow a where
    width :: a -> Int
    getBit :: Int -> a -> Bit
    eliminateWith :: Int -> a -> a -> a

instance TableauRow Pauli where
    width :: Pauli -> Int
    width = vectorLength

    getBit :: Int -> Pauli -> Bit
    getBit = bitAt

    eliminateWith :: Int -> Pauli -> Pauli -> Pauli
    eliminateWith col pivot row
        | getBit col row == 1 = groupOp pivot row
        | otherwise           = row

instance TableauRow (Pauli, Pauli) where
    width :: (Pauli, Pauli) -> Int
    width (p1, p2) = vectorLength p1 + vectorLength p2

    getBit :: Int -> (Pauli, Pauli) -> Bit
    getBit i (p1, p2)
        | i < width p1       = bitAt i p1
        | i < width (p1, p2) = bitAt (i - width p1) p2
        | otherwise          = error ("Error: getBit: index out of bounds: " ++ show i ++ " for BiTableau with " ++ show (width p1, width p2) ++ " qubits")

    eliminateWith :: Int -> (Pauli, Pauli) -> (Pauli, Pauli) -> (Pauli, Pauli)
    eliminateWith col (pivot1, pivot2) (row1, row2)
        | getBit col (row1, row2) == 1 = (groupOp pivot1 row1, groupOp pivot2 row2)
        | otherwise                    = (row1, row2)

-----------------------------
-- Constans
-----------------------------
infeasibleTableau :: Int -> StabTableau
infeasibleTableau n = Tableau n [negIdentityPauli n]




normalize :: StabTableau -> StabTableau
normalize (Tableau n gens)
    | inf `elem` gens' = infeasibleTableau n
    | otherwise        = Tableau n (removeZeroRows gens')
    where
        inf = head (generators (infeasibleTableau n))
        gens' = toEchelonFormWithPivot 0 gens

removeZeroRows :: [Pauli] -> [Pauli]
removeZeroRows = takeWhile (not . isIdentity)

stackVertical :: StabTableau -> StabTableau -> StabTableau
stackVertical (Tableau n gens) (Tableau n' gens')
    | n /= n'  = error ("Tableaux must have the same number of qubits for stacking: " ++ show n ++ " vs " ++ show n')
    | otherwise = Tableau n (gens ++ gens')

-----------------------------
-- Gaussian elimination
-----------------------------
toEchelonForm :: StabTableau -> StabTableau
toEchelonForm (Tableau n gens) = Tableau n (toEchelonFormWithPivot 0 gens)

toEchelonFormBi :: BiTableau -> BiTableau
toEchelonFormBi (BiTableau (n, n') gens) = BiTableau (n, n') (toEchelonFormWithPivot 0 gens)

toEchelonFormWithPivot :: (TableauRow a) => Int -> [a] -> [a]
toEchelonFormWithPivot _ [] = []
toEchelonFormWithPivot _ [r] = [r]
toEchelonFormWithPivot col (r:rs)
    | isNothing h = toEchelonFormWithPivot (col + 1) (r:rs)
    | h == Just 0 = r : toEchelonFormWithPivot (col + 1) (eliminateBelowPivot col (r:rs))
    | otherwise   = r' : toEchelonFormWithPivot (col + 1) (eliminateBelowPivot col (r':r's))
    where
        h = firstRowWithOneInCol col (r:rs)
        (r':r's) = swapHeadWithRow (extractValue h) (r:rs)

firstRowWithOneInCol :: (TableauRow a) => Int -> [a] -> Maybe Int
firstRowWithOneInCol = findFirstRowWithOneInColFrom 0

findFirstRowWithOneInColFrom :: (TableauRow a) => Int -> Int -> [a] -> Maybe Int
findFirstRowWithOneInColFrom _ _ [] = Nothing
findFirstRowWithOneInColFrom ind col (r:rs)
    | getBit col r == 1 = Just ind
    | otherwise         = findFirstRowWithOneInColFrom (ind + 1) col rs

swapHeadWithRow :: Int -> [a] -> [a]
swapHeadWithRow _ [] = []
swapHeadWithRow 0 xs = xs
swapHeadWithRow i (x:xs) = y : before ++ [x] ++ after
    where
        (before, y:after) = splitAt (i - 1) xs

eliminateBelowPivot :: (TableauRow a) => Int -> [a] -> [a]
eliminateBelowPivot col (pivot:rows) = map (eliminateWith col pivot) rows

-----------------------------
-- Projection
-----------------------------
-- projectOut :: Int -> StabTableau -> StabTableau
-- projectOut col (Tableau n gens) = Tableau (n - col) (map projectOutFromGen gens)
--     where
--         projectOutFromGen :: Pauli -> Pauli
--         projectOutFromGen p
--             | bitAt col p == 1 = groupOp p (generators (toEchelonForm (Tableau n gens)) !! col)
--             | otherwise        = p




-- projectOut (col, val) (Tableau n gens) = Tableau n (map projectOutFromGen gens)
--     where
--         projectOutFromGen :: Pauli -> Pauli
--         projectOutFromGen p
--             | bitAt col p == val = groupOp p (generators (toEchelonForm (Tableau n gens)) !! col)
--             | otherwise          = p

-- Current Pauli and Tableau design is ok.
-- Then, define the lattice and Kleene algebra modules.
-- Then see how you'd need to implement projection to be used by all of them.

-----------------------------
-- Intersection and union
-----------------------------
-- intersection :: StabTableau -> StabTableau -> StabTableau
-- intersection (Tableau n1 gens1) (Tableau n2 gens2)
--     | n1 /= n2  = error ("Tableaux must have the same number of qubits for intersection: " ++ show n1 ++ " vs " ++ show n2)
--     | otherwise = projectOut (n1, 0) (Tableau n1 (gens1 ++ gens2))
--         where

-- -- if -I
-- intersect :: [Pauli] -> [Pauli] -> [Pauli]
-- intersect gens1 gens2 = 
--     where
--         gens' = (map (\g -> tensorProduct g g) gens1) ++
--                 (map (\g -> tensorProduct g identityPauli n2) gens2)

-----------------------------
-- Testing
-----------------------------
tableauToString :: StabTableau -> String
tableauToString Tableau {qubitNum = _, generators = paulies} =
    unlines $ map pauliToString paulies

nxz = pauliFromString "-XZ"
zx = pauliFromString "ZX"
nyy = pauliFromString "-YY"

tab = Tableau {qubitNum = 2, generators = [zx, nyy, nxz]}

-- p1 = pauliFromString "-ZIXY"
-- p2 = pauliFromString "IZYX"
-- p3 = pauliFromString "-ZZZZ"

-- tab2 = Tableau {qubitNum = 4, generators = [p1, p2, p3]}

xz = pauliFromString "XZ"
-- zx = pauliFromString "ZX"
t1 = Tableau {qubitNum = 2, generators = [xz, zx]}

nxx = pauliFromString "-XX"
zz = pauliFromString "ZZ"
t2 = Tableau {qubitNum = 2, generators = [nxx, zz]}

bt = BiTableau {qubitNums = (2, 2), biGenerators = [(xz, nxx), (zx, zz)]}


zi = pauliFromString "ZI"
iz = pauliFromString "IZ"
ix = pauliFromString "IX"
ii = pauliFromString "II"

bt2 = BiTableau {qubitNums = (2, 2), biGenerators = [(zi, zi), (ix, ix), (zi, ii), (iz, ii)]}


-- TODO: intersection and union
