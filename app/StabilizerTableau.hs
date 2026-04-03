{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module StabilizerTableau
( StabTableau(..)
, BiTableau(..)
, trivialTableau
, unsatTableau
, intersection
, union
, normalize
, projectOutBy
, sTab
, zTab
, gTab
, hTab
) where

import PauliOperator
import Utils (extractValue)

import Data.Maybe (isNothing, mapMaybe)
import Data.List (takeWhile)
import Data.Bit ( Bit )
import Data.Bits (xor)

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
        | otherwise          = error ("Error: getBit: index out of bounds: " ++ show i ++
                                        " for BiTableau with width " ++ show (width (p1, p2)))

    eliminateWith :: Int -> (Pauli, Pauli) -> (Pauli, Pauli) -> (Pauli, Pauli)
    eliminateWith col (pivot1, pivot2) (row1, row2)
        | getBit col (row1, row2) == 1 = (groupOp pivot1 row1, groupOp pivot2 row2)
        | otherwise                    = (row1, row2)

-----------------------------
-- Constans
-----------------------------
zeroTableau :: Int -> Int -> StabTableau
zeroTableau n k = Tableau {qubitNum = n, generators = replicate k (identityPauli n)}

trivialTableau :: Int -> StabTableau
trivialTableau n = zeroTableau n 1

isTrivial :: StabTableau -> Bool
isTrivial (Tableau n gens) = null gens -- || all isIdentity gens

unsatTableau :: Int -> StabTableau
unsatTableau n = Tableau {qubitNum = n, generators = [negIdentityPauli n]}

isUnsat :: StabTableau -> Bool
isUnsat (Tableau n gens) = negIdentityPauli n `elem` gens || any ((== 1) . iBit) gens

-----------------------------
-- Operations
-----------------------------
-- Precondition: both tableaux are normalized
intersection :: StabTableau -> StabTableau -> StabTableau
intersection t1@(Tableau n1 gen1) t2@(Tableau n2 gen2)
    | isUnsat t1 = t2
    | isUnsat t2 = t1
    | n1 /= n2   = error ("Tableaux must have the same number of qubits for intersection: " ++ show n1 ++ " vs " ++ show n2)
    | otherwise  = projectOutBy projectMap BiTableau {qubitNums = (n1, n2), biGenerators = zip leftGens rightGens}
    where
        leftGens = gen1 ++ gen2
        rightGens = gen1 ++ replicate (length gen2) (identityPauli n1)
        projectMap (p1, p2) = if isIdentity p1 then Just p2 else Nothing

union :: StabTableau -> StabTableau -> StabTableau
union t1@(Tableau n1 gen1) t2@(Tableau n2 gen2)
    | isTrivial t1 = t2
    | isTrivial t2 = t1
    | n1 /= n2     = error ("Tableaux must have the same number of qubits for union: " ++ show n1 ++ " vs " ++ show n2)
    | otherwise    = normalize Tableau {qubitNum = n1, generators = gen1 ++ gen2}

-----------------------------
-- Normalization
-----------------------------
normalize :: StabTableau -> StabTableau
normalize (Tableau n gens)
    | isTrivial t' = trivialTableau n
    | isUnsat t'   = unsatTableau n
    | otherwise    = t'
    where
        gens' = takeWhile (not . isIdentity) $ toEchelonFormWithPivot 0 gens
        t' = Tableau {qubitNum = n, generators = gens'}

projectOutBy :: ((Pauli, Pauli) -> Maybe Pauli) -> BiTableau -> StabTableau
projectOutBy extract (BiTableau (_, n') biGens) = normalize Tableau {qubitNum = n', generators = mapMaybe extract biGens'}
    where
        biGens' = toEchelonFormWithPivot 0 biGens

toEchelonForm :: StabTableau -> StabTableau
toEchelonForm (Tableau n gens) = Tableau {qubitNum = n, generators = toEchelonFormWithPivot 0 gens}

toEchelonFormBi :: BiTableau -> BiTableau
toEchelonFormBi (BiTableau (n, n') gens) = BiTableau {qubitNums = (n, n'), biGenerators = toEchelonFormWithPivot 0 gens}

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

-- Testing
tableauToString :: StabTableau -> String
tableauToString Tableau {qubitNum = _, generators = paulies} =
    unlines $ map pauliToString paulies

ii :: Pauli
ii = pauliFromString "II"
ix :: Pauli
ix = pauliFromString "IX"
iz :: Pauli
iz = pauliFromString "IZ"
zi :: Pauli
zi = pauliFromString "ZI"
xz :: Pauli
xz = pauliFromString "XZ"
nxz :: Pauli
nxz = pauliFromString "-XZ"
zx :: Pauli
zx = pauliFromString "ZX"
xx :: Pauli
xx = pauliFromString "XX"
nxx :: Pauli
nxx = pauliFromString "-XX"
zz :: Pauli
zz = pauliFromString "ZZ"
yy :: Pauli
yy = pauliFromString "YY"
nyy :: Pauli
nyy = pauliFromString "-YY"
xy :: Pauli
xy = pauliFromString "XY"
p1 :: Pauli
p1 = pauliFromString "YZYXYZ"
p2 :: Pauli
p2 = pauliFromString "ZIZYYY"
p3 :: Pauli
p3 = pauliFromString "XYZYYY"
p4 :: Pauli
p4 = pauliFromString "ZIXYIY"

tab :: StabTableau
tab = Tableau {qubitNum = 2, generators = [zx, nyy, nxz]}
-- tab2 = Tableau {qubitNum = 4, generators = [p1, p2, p3]}
t1 :: StabTableau
t1 = Tableau {qubitNum = 2, generators = [xz, zx]}
t2 :: StabTableau
t2 = Tableau {qubitNum = 2, generators = [nxx, zz]}
sTab :: StabTableau
sTab = Tableau {qubitNum = 2, generators = [xy, zz]}
zTab :: StabTableau
zTab = Tableau {qubitNum = 2, generators = [nxx, zz]}

gTab :: StabTableau
gTab = Tableau {qubitNum = 6, generators = [p1, p2]}
hTab :: StabTableau
hTab = Tableau {qubitNum = 6, generators = [p3, p4]}

bt :: BiTableau
bt = BiTableau {qubitNums = (2, 2), biGenerators = [(xz, nxx), (zx, zz)]}
bt2 :: BiTableau
bt2 = BiTableau {qubitNums = (2, 2), biGenerators = [(zi, zi), (ix, ix), (zi, ii), (iz, ii)]}
