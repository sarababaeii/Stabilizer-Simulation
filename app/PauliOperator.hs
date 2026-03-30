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
, groupOp
, tensorProduct
, applyGate
, measuringPauli
, isInCompBasis
, firstNonCompBasisPauli
, pauliToString
, pauliFromString
) where

import Quantum (Gate(..), Measure (qubit))
import BitUtils

import Data.Bits
import Data.Bit
import Data.BitVector

import System.Random (StdGen)
import Control.DeepSeq (NFData(..))
import GHC.Generics (Generic)

data Pauli = Pauli { xBits :: BitVector
                   , zBits :: BitVector
                   , phaseBit :: Bit
                   } deriving (Generic, Eq, Read)

instance Show Pauli where
    show :: Pauli -> String
    show (Pauli xs zs c) =
        "{" ++ show (bvToBits xs) ++ " | " ++ show (bvToBits zs) ++ " | " ++ show c ++ "}"

instance NFData BitVector where
    rnf :: BitVector -> ()
    rnf bv = bv `seq` ()

instance NFData Pauli

qubitCount :: Pauli -> Int
qubitCount p = size (xBits p)

vectorLength :: Pauli -> Int
vectorLength p = 2 * qubitCount p + 1

phaseBool :: Pauli -> Bool
phaseBool p = unBit (phaseBit p)

bitAt :: Int -> Pauli -> Bit
bitAt i (Pauli xs zs c)
  | i < n      = boolToBit (xs !. i)
  | i < 2 * n  = boolToBit (zs !. (i - n))
  | i == 2 * n = c
  | otherwise  = error ("Error: bitAt: index out of bounds: " ++ show i ++ " for Pauli with " ++ show n ++ " qubits")
  where
    n = size xs

---------------------------------------
-- Constants
---------------------------------------
identityPauli :: Int -> Pauli
identityPauli n = Pauli z z 0
    where
        z = zeroBV n

negIdentityPauli :: Int -> Pauli
negIdentityPauli n = Pauli z z 1
    where
        z = zeroBV n

xPauli :: Int -> Int -> Pauli
xPauli n i = Pauli xs zs 0
    where
        xs = singleOneBV n i
        zs = zeroBV n

zPauli :: Int -> Int -> Pauli
zPauli n i = Pauli xs zs 0
    where
        xs = zeroBV n
        zs = singleOneBV n i

isIdentity :: Pauli -> Bool
isIdentity (Pauli xs zs c) = xs == 0 && zs == 0 && c == 0

-----------------------------
-- Operations
-----------------------------
-- (P, Q -> PQ)
-- different from the paper
groupOp :: Pauli -> Pauli -> Pauli
groupOp p1 p2 = Pauli xs' zs' c'
    where
        c' = phaseAfterGroupOp p1 p2
        xs' = xBits p1 `xor` xBits p2
        zs' = zBits p1 `xor` zBits p2

phaseAfterGroupOp :: Pauli -> Pauli -> Bit
phaseAfterGroupOp p1 p2
    | c' `mod` 4 == 0 = 0
    | c' `mod` 4 == 2 = 1
    | otherwise       = error ("Error: Paulies don't commute: " ++ show p1 ++ " and " ++ show p2)
    where
        n = qubitCount p1
        gs = [generatedPhase (xBits p1 !. i) (zBits p1 !. i) 
                (boolToInt (xBits p2 !. i)) (boolToInt (zBits p2 !. i)) | i <- [0 .. n - 1]]
        c' = 2 * bitToInt (phaseBit p1) + 2 * bitToInt (phaseBit p2) + sum gs

generatedPhase :: Bool -> Bool -> Int -> Int -> Int
generatedPhase False False _ _  = 0
generatedPhase False True x2 z2 = x2 * (1 - 2 * z2)
generatedPhase True False x2 z2 = z2 * (2 * x2 - 1)
generatedPhase True True x2 z2 = z2 - x2

tensorProduct :: Pauli -> Pauli -> Pauli
tensorProduct (Pauli xs1 zs1 c1) (Pauli xs2 zs2 c2) = Pauli xs zs c
    where
        xs = xs1 # xs2
        zs = zs1 # zs2
        c = c1 `xor` c2

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
    in Pauli {xBits = xs', zBits = zs', phaseBit = r'}
applyGate H {target = a} p =
    let xs = xBits p
        zs = zBits p
        r = phaseBit p
        (xa, za) = selectedBits a p
        r' = r `xor` (xa .&. za)
        xs' = replaceBit a za xs
        zs' = replaceBit a xa zs
    in Pauli {xBits = xs', zBits = zs', phaseBit = r'}
applyGate S {target = a} p =
    let xs = xBits p
        zs = zBits p
        r = phaseBit p
        (xa, za) = selectedBits a p
        r' = r `xor` (xa .&. za)
        za' = xa `xor` za
        zs' = replaceBit a za' zs
    in Pauli {xBits = xs, zBits = zs', phaseBit = r'}

selectedBits :: Int -> Pauli -> (Bit, Bit)
selectedBits i (Pauli xs zs _) = (x, z)
    where
        x = boolToBit (xs !. i)
        z = boolToBit (zs !. i)

measuringPauli :: Pauli -> Int -> StdGen -> (Pauli, StdGen)
measuringPauli p a gen = (Pauli xs zs c, gen')
    where
        n = qubitCount p
        xs = zeroBV n
        zs = zeroExtend a (bit (n - 1 - a))
        (c, gen') = randomBit gen

isInCompBasis :: Int -> Pauli -> Bool
isInCompBasis i p = Prelude.not x
    where
         x = xBits p !. i

firstNonCompBasisPauli :: Int -> [Pauli] -> Maybe (Pauli, Int)
firstNonCompBasisPauli = findFirstNonCompBasisPauli 0

findFirstNonCompBasisPauli :: Int -> Int -> [Pauli] -> Maybe (Pauli, Int)
findFirstNonCompBasisPauli _ _ [] = Nothing
findFirstNonCompBasisPauli ind a (p:ps)
    | isInCompBasis a p = findFirstNonCompBasisPauli (ind + 1) a ps
    | otherwise         = Just (p, ind)

-- Testing
pauliToString :: Pauli -> String
pauliToString (Pauli xs zs c) = sign ++ body
    where
        n = size xs
        sign = if c == 1 then "-" else ""
        body = [pauliChar (xs !. i) (zs !. i) | i <- [0 .. n - 1]]

pauliChar :: Bool -> Bool -> Char
pauliChar False False = 'I'
pauliChar True False = 'X'
pauliChar False True = 'Z'
pauliChar True True = 'Y'

pauliFromString :: String -> Pauli
pauliFromString str = Pauli xs zs c
    where
        (c, body) = parseSign str
        xs = fromBits [x | chr <- body, let (x, _) = pauliBits chr]
        zs = fromBits [z | chr <- body, let (_, z) = pauliBits chr]

pauliBits :: Char -> (Bool, Bool) -- can I convert Bool to Bit?
pauliBits 'I' = (False, False)
pauliBits 'X' = (True , False)
pauliBits 'Z' = (False, True )
pauliBits 'Y' = (True , True )
pauliBits chr   = error ("Error: pauliBits: invalid Pauli character: " ++ show chr)

parseSign :: String -> (Bit, String)
parseSign ""         = error "Error: parseSign: invalid Pauli string: empty string"
parseSign ('-':[])   = error "Error: parseSign: invalid Pauli string: empty string"
parseSign ('-':xs)   = (1, xs)
parseSign xs         = (0, xs)
