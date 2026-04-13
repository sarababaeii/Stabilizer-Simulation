{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module PauliOperator
( Pauli(..)
, qubitCount
, vectorLength
, phaseBool
, bitAt
, identityPauli
, negIdentityPauli
, xPauli
, zPauli
, isIdentity
, isLikeIdentity
, groupOp
, phaseAfterGroupOp
, tensorProduct
, splitInOutPauli
, transposedPhase
, applyGate
, measuringPauli
, isInCompBasis
, firstNonCompBasisPauli
, pauliToString
, pauliFromString
) where

import Quantum (Gate(..), Measure (qubit))
import BitUtils

import Data.Bit (Bit)
import Data.Bits (bit, xor, (.&.))
import Data.BitVector (BitVector, (!.), (#), size, fromBits, zeroExtend)

import System.Random (StdGen)
import Control.DeepSeq (NFData(..))
import GHC.Generics (Generic)
import Data.Type.Coercion (trans)

data Pauli = Pauli { xBits :: BitVector
                   , zBits :: BitVector
                   , phaseBit :: Bit
                   , iBit :: Bit
                   } deriving (Generic, Eq, Read)

instance Show Pauli where
    show :: Pauli -> String
    show (Pauli xs zs c d) =
        "{" ++ show (bvToBits xs) ++ " | " ++ show (bvToBits zs) ++ " | " ++
                show c ++ " | " ++ show d ++ "}"

instance NFData BitVector where
    rnf :: BitVector -> ()
    rnf bv = bv `seq` ()

instance NFData Pauli

-----------------------------
-- Accessors
-----------------------------
qubitCount :: Pauli -> Int
qubitCount p = size (xBits p)

vectorLength :: Pauli -> Int
vectorLength p = 2 * qubitCount p + 2

phaseBool :: Pauli -> Bool
phaseBool (Pauli _ _ c _) = bitToBool c

bitAt :: Int -> Pauli -> Bit
bitAt i p@(Pauli xs zs c d)
  | i < n          = boolToBit (xs !. i)
  | i < 2 * n      = boolToBit (zs !. (i - n))
  | i == 2 * n     = c
  | i == 2 * n + 1 = d
  | otherwise  = error ("Error: bitAt: index out of bounds: " ++ show i ++ 
                            " for Pauli with " ++ show n ++ " qubits")
  where
    n = qubitCount p

---------------------------------------
-- Constants
---------------------------------------
identityPauli :: Int -> Pauli
identityPauli n = Pauli {xBits = z, zBits = z, phaseBit = 0, iBit = 0}
    where
        z = zeroBV n

negIdentityPauli :: Int -> Pauli
negIdentityPauli n = Pauli {xBits = z, zBits = z, phaseBit = 1, iBit = 0}
    where
        z = zeroBV n

xPauli :: Int -> Int -> Pauli
xPauli n i = Pauli {xBits = xs, zBits = zs, phaseBit = 0, iBit = 0}
    where
        xs = singleOneBV n i
        zs = zeroBV n

zPauli :: Int -> Int -> Pauli
zPauli n i = Pauli {xBits = xs, zBits = zs, phaseBit = 0, iBit = 0}
    where
        xs = zeroBV n
        zs = singleOneBV n i

isIdentity :: Pauli -> Bool
isIdentity (Pauli xs zs c d) = xs == 0 && zs == 0 && c == 0 && d == 0

isLikeIdentity :: Pauli -> Bool
isLikeIdentity (Pauli xs zs _ _) = xs == 0 && zs == 0

-----------------------------
-- Operations
-----------------------------
-- P, Q -> PQ (different from the Improved Stab Sim paper)
groupOp :: Pauli -> Pauli -> Pauli
groupOp p1@(Pauli xs1 zs1 _ _) p2@(Pauli xs2 zs2 _ _)
    | n1 /= n2  = error ("Error: groupOp: Paulies must have the same number of qubits for group operation: " ++ show n1 ++ " vs " ++ show n2)
    | otherwise = Pauli {xBits = xs', zBits = zs', phaseBit = c', iBit = d'}
    where
        (n1, n2) = (qubitCount p1, qubitCount p2)
        (c', d') = phaseAfterGroupOp n1 p1 p2
        xs' = xs1 `xor` xs2
        zs' = zs1 `xor` zs2

phaseAfterGroupOp :: Int -> Pauli -> Pauli -> (Bit, Bit)
phaseAfterGroupOp n (Pauli xs1 zs1 c1 d1) (Pauli xs2 zs2 c2 d2)
    | p `mod` 4 == 0 = (0, 0)
    | p `mod` 4 == 2 = (1, 0)
    | p `mod` 4 == 1 = (0, 1)
    | p `mod` 4 == 3 = (1, 1)
    where
        gs = [generatedPhase (xs1 !. i) (zs1 !. i) (xs2 !. i) (zs2 !. i) | i <- [0 .. n - 1]]
        p = sum gs + bitPairToInt c1 d1 + bitPairToInt c2 d2 

generatedPhase :: Bool -> Bool -> Bool -> Bool -> Int
generatedPhase x1 z1 x2 z2
    | not x1 && not z1 = 0
    | not x1 &&     z1 = ix2 * (1 - 2 * iz2)
    |     x1 && not z1 = iz2 * (2 * ix2 - 1)
    |     x1 &&     z1 = iz2 - ix2
    where
        ix2 = boolToInt x2
        iz2 = boolToInt z2

tensorProduct :: Pauli -> Pauli -> Pauli
tensorProduct (Pauli xs1 zs1 c1 d1) (Pauli xs2 zs2 c2 d2) =
                Pauli {xBits = xs, zBits = zs, phaseBit = c, iBit = d}
    where
        xs = xs1 # xs2
        zs = zs1 # zs2
        c = c1 `xor` c2 `xor` (d1 .&. d2)
        d = d1 `xor` d2

-- cd P_1 \otimes ... \otimes P_{2n} -> 
--     (P_1 \otimes ... \otimes P_n, cd P_{n+1} \otimes ... \otimes P_{2n})
splitInOutPauli :: Pauli -> (Pauli, Pauli)
splitInOutPauli p@(Pauli xs zs c d)
    | odd n = error ("Error: splitInOutPauli: Pauli must have even number of qubits: " ++ show n)
    | otherwise = (Pauli {xBits = xIn, zBits = zIn, phaseBit = 0, iBit = 0},
                    Pauli {xBits = xOut, zBits = zOut, phaseBit = c, iBit = d})
    where
        n = qubitCount p
        (xIn, xOut) = splitAtBV (n `div` 2) xs
        (zIn, zOut) = splitAtBV (n `div` 2) zs

-- TODO: i?
transposedPhase :: Pauli -> Bit
transposedPhase (Pauli xs zs c _)
    | odd yCount = c `xor` 1
    | otherwise  = c
    where
        yCount = oneCount (xs .&. zs)

-----------------------------
-- Simulation
-----------------------------
applyGate :: Gate -> Pauli -> Pauli
applyGate CX {control = a, target = b} p =
    let xs = xBits p
        zs = zBits p
        r = phaseBit p
        (xa, za) = selectedBits a p
        (xb, zb) = selectedBits b p
        r' = r `xor` ((xa .&. zb) .&. ((xb `xor` za) `xor` 1))
        xb' = xa `xor` xb
        xs' = replaceBit b xb' xs
        za' = za `xor` zb
        zs' = replaceBit a za' zs
    in Pauli {xBits = xs', zBits = zs', phaseBit = r', iBit = 0}
applyGate H {target = a} p =
    let xs = xBits p
        zs = zBits p
        r = phaseBit p
        (xa, za) = selectedBits a p
        r' = r `xor` (xa .&. za)
        xs' = replaceBit a za xs
        zs' = replaceBit a xa zs
    in Pauli {xBits = xs', zBits = zs', phaseBit = r', iBit = 0}
applyGate S {target = a} p =
    let xs = xBits p
        zs = zBits p
        r = phaseBit p
        (xa, za) = selectedBits a p
        r' = r `xor` (xa .&. za)
        za' = xa `xor` za
        zs' = replaceBit a za' zs
    in Pauli {xBits = xs, zBits = zs', phaseBit = r', iBit = 0}

selectedBits :: Int -> Pauli -> (Bit, Bit)
selectedBits i (Pauli xs zs _ _) = (x, z)
    where
        x = boolToBit (xs !. i)
        z = boolToBit (zs !. i)

measuringPauli :: Pauli -> Int -> StdGen -> (Pauli, StdGen)
measuringPauli p a gen = (Pauli {xBits = xs, zBits = zs, phaseBit = c, iBit = 0}, gen')
    where
        n = qubitCount p
        xs = zeroBV n
        zs = zeroExtend a (bit (n - 1 - a))
        (c, gen') = randomBit gen

isInCompBasis :: Int -> Pauli -> Bool
isInCompBasis i (Pauli xs _ _ _) = not $ xs !. i

firstNonCompBasisPauli :: Int -> [Pauli] -> Maybe (Pauli, Int)
firstNonCompBasisPauli = findFirstNonCompBasisPauli 0

findFirstNonCompBasisPauli :: Int -> Int -> [Pauli] -> Maybe (Pauli, Int)
findFirstNonCompBasisPauli _ _ [] = Nothing
findFirstNonCompBasisPauli ind a (p:ps)
    | isInCompBasis a p = findFirstNonCompBasisPauli (ind + 1) a ps
    | otherwise         = Just (p, ind)

-- Testing
pauliToString :: Pauli -> String
pauliToString p@(Pauli xs zs c d) = phase ++ iPart ++ body
    where
        n = qubitCount p
        phase = if c == 1 then "-" else ""
        iPart = if d == 1 then "i" else ""
        body = [pauliChar (xs !. i) (zs !. i) | i <- [0 .. n - 1]]

pauliChar :: Bool -> Bool -> Char
pauliChar False False = 'I'
pauliChar True False = 'X'
pauliChar False True = 'Z'
pauliChar True True = 'Y'

pauliFromString :: String -> Pauli
pauliFromString str = Pauli {xBits = xs, zBits = zs, phaseBit = c, iBit = d}
    where
        (c, d, body) = parsePhase str
        xs = fromBits [x | chr <- body, let (x, _) = pauliBits chr]
        zs = fromBits [z | chr <- body, let (_, z) = pauliBits chr]

pauliBits :: Char -> (Bool, Bool) -- can I convert Bool to Bit?
pauliBits 'I' = (False, False)
pauliBits 'X' = (True , False)
pauliBits 'Z' = (False, True )
pauliBits 'Y' = (True , True )
pauliBits chr   = error ("Error: pauliBits: invalid Pauli character: " ++ show chr)

parsePhase :: String -> (Bit, Bit, String)
parsePhase ('i':xs)     = (0, 1, xs)
parsePhase ('-':'i':xs) = (1, 1, xs)
parsePhase ('-':xs)     = (1, 0, xs)
parsePhase xs           = (0, 0, xs)
