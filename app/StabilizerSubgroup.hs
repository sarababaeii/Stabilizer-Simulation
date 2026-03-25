module StabilizerSubgroup where

import PauliOperator

import Data.Maybe (isNothing)

data StabSubgroup = Tableau { qubitNum :: Int
                            , generators :: [Pauli]
                            } deriving (Eq, Show, Read)

-- Gaussian elimination
toEchelonForm :: StabSubgroup -> StabSubgroup
toEchelonForm Tableau {qubitNum = n, generators = gens} = Tableau {qubitNum = n, generators = toEchelonFormWithPivot 0 gens}

toEchelonFormWithPivot :: Int -> [Pauli] -> [Pauli]
toEchelonFormWithPivot _ [] = []
toEchelonFormWithPivot _ [p] = [p]
toEchelonFormWithPivot c (p:ps)
    | isNothing r = toEchelonFormWithPivot (c + 1) (p:ps)
    | r == Just 0 = p : toEchelonFormWithPivot (c + 1) (eliminateBelowPivot c (p:ps))
    | otherwise   = p' : toEchelonFormWithPivot (c + 1) (eliminateBelowPivot c (p':p's))
    where
        r = firstRowWithOneInCol c (p:ps)
        (p':p's) = swapPivotWithRow (extractValue r) (p:ps)

firstRowWithOneInCol :: Int -> [Pauli] -> Maybe Int
firstRowWithOneInCol = findFirstRowWithOneInColFrom 0

findFirstRowWithOneInColFrom :: Int -> Int -> [Pauli] -> Maybe Int
findFirstRowWithOneInColFrom _ _ [] = Nothing
findFirstRowWithOneInColFrom ind c (p:ps)
    | bitAt c p == 1 = Just ind
    | otherwise      = findFirstRowWithOneInColFrom (ind + 1) c ps

swapPivotWithRow :: Int -> [a] -> [a]
swapPivotWithRow _ [] = []
swapPivotWithRow 0 xs = xs
swapPivotWithRow i (x:xs) = y : before ++ [x] ++ after
    where
        (before, y:after) = splitAt (i - 1) xs

eliminateBelowPivot :: Int -> [Pauli] -> [Pauli]
eliminateBelowPivot c (p:ps) = map (\q -> if bitAt c q == 1 then groupOp p q else q) ps

-- Testing
showStabSubgroup :: StabSubgroup -> String
showStabSubgroup Tableau {qubitNum = _, generators = paulies} =
    showPaulies paulies

tableauToString :: StabSubgroup -> String
tableauToString Tableau {qubitNum = _, generators = paulies} =
    unlines $ map pauliToString paulies

extractValue :: Maybe a -> a
extractValue (Just x) = x

nxz = pauliFromString "-XZ"
zx = pauliFromString "ZX"
nyy = pauliFromString "-YY"

tab = Tableau {qubitNum = 2, generators = [zx, nyy, nxz]}