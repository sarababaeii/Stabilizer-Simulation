{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module PauliOperator
( Pauli(..)
, phaseBool
, identityPauli
, xPauli
, zPauli
, measuringPauli
, applyGate
, groupOp
, isInCompBasis
, firstNonCompBasisPauli
, showPaulies
) where

import Quantum (Gate(..))

import Data.Bits
import Data.Bit
import Data.BitVector
import System.Random (StdGen, random)
import Control.DeepSeq (NFData(..))
import GHC.Generics (Generic)

data Pauli = Pauli { xBits :: BitVector
                   , zBits :: BitVector
                   , phaseBit :: Bit
                   } deriving (Generic, Eq, Show, Read)

instance NFData BitVector where
    rnf bv = bv `seq` ()
instance NFData Pauli

phaseBool :: Pauli -> Bool
phaseBool p = unBit (phaseBit p)

identityPauli :: Int -> Pauli
identityPauli n = Pauli {xBits = z, zBits = z, phaseBit = 0}
    where
        z = zeroBV n

xPauli :: Int -> Int -> Pauli
xPauli n i = Pauli {xBits = xs, zBits = zs, phaseBit = 0}
    where
        xs = singleOnBV n i
        zs = zeroBV n

zPauli :: Int -> Int -> Pauli
zPauli n i = Pauli {xBits = xs, zBits = zs, phaseBit = 0}
    where
        xs = zeroBV n
        zs = singleOnBV n i

-----------------------------
-- Operations
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
selectedBits i Pauli {xBits = xs, zBits = zs, phaseBit = _} = (x, z)
    where 
        i' = size xs - 1 - i
        x = toBit (xs @. i')
        z = toBit (zs @. i')

-- (P, Q -> PQ)
-- different from the paper
groupOp :: Pauli -> Pauli -> Pauli
groupOp p1 p2 =
    let r' = phaseAfterGroupOp p1 p2
        xs' = (xBits p1) `xor` (xBits p2)
        zs' = (zBits p1) `xor` (zBits p2)
    in Pauli {xBits = xs', zBits = zs', phaseBit = r'}

phaseAfterGroupOp :: Pauli -> Pauli -> Bit
phaseAfterGroupOp Pauli {xBits = xs1, zBits = zs1, phaseBit = r1} Pauli {xBits = xs2, zBits = zs2, phaseBit = r2} 
    | r' `mod` 4 == 0 = 0
    | r' `mod` 4 == 2 = 1
    where 
        gs = [g (xs1 @. i) (zs1 @. i) (toInt (xs2 @. i)) (toInt (zs2 @. i)) | i <- [0 .. (size xs1) - 1]]
        gSum = sum gs
        r' = 2 * (toInt' r1) + 2 * (toInt' r2) + gSum

g :: Bool -> Bool -> Int -> Int -> Int
g False False _ _  = 0
g False True x2 z2 = x2 * (2 * z2 - 1)
g True False x2 z2 = z2 * (2 * x2 - 1)
g True True x2 z2 = z2 - x2

measuringPauli :: Pauli -> Int -> StdGen -> (Pauli, StdGen)
measuringPauli p a gen = (Pauli {xBits = xs, zBits = zs, phaseBit = r}, gen')
    where 
        n = size (xBits p)
        xs = zeroBV n
        zs = zeroExtend a (bit (n - 1 - a))
        (r, gen') = randomBit gen

isInCompBasis :: Int -> Pauli -> Bool
isInCompBasis i p = 
    let n = size (xBits p)
        x = (xBits p) @. (n - 1 - i)
    in x == False

firstNonCompBasisPauli :: Int -> [Pauli] -> Maybe (Pauli, Int)
firstNonCompBasisPauli a ps = findFirstNonCompBasisPauli 0 a ps

findFirstNonCompBasisPauli :: Int -> Int -> [Pauli] -> Maybe (Pauli, Int)
findFirstNonCompBasisPauli _ _ [] = Nothing
findFirstNonCompBasisPauli ind a (p:ps) 
    | isInCompBasis a p = findFirstNonCompBasisPauli (ind + 1) a ps 
    | otherwise         = Just (p, ind)

---------------------------------------
-- Auxiliary functions
---------------------------------------
replaceBit :: Int -> Bit -> BitVector -> BitVector
replaceBit i 1 bv = setBit bv (size bv - 1 - i)
replaceBit i 0 bv = clearBit' bv (size bv - 1 - i) 

clearBit' :: BitVector -> Int -> BitVector  -- O(4n)
clearBit' bv i =
    let nZero = bv .&. zeroBits     -- O(n)
        p = setBit nZero i          -- O(n)
    in bv .&. (complement p)        -- O(2n)

zeroBV :: Int -> BitVector
zeroBV n = t `xor` t
    where t = singleOnBV n (n - 1)

singleOnBV :: Int -> Int -> BitVector
singleOnBV n i = zeroExtend i (bit (n - i - 1))

toBit :: Bool -> Bit
toBit False = 0
toBit True = 1

toInt :: Bool -> Int
toInt False = 0
toInt True = 1

toInt' :: Bit -> Int
toInt' 0 = 0
toInt' 1 = 1

randomBit :: StdGen -> (Bit, StdGen)
randomBit gen = (toBit b, gen')
    where (b, gen') = random gen :: (Bool, StdGen)

-- Testing
showPauli :: Pauli -> String
showPauli Pauli {xBits = xs, zBits = zs, phaseBit = r} =
    "[ " ++ show (toBits xs) ++ " | " ++ show (toBits zs) ++ " | " ++ show r ++ " ]" ++ "\n"

showPaulies :: [Pauli] -> String
showPaulies [] = ";\n"
showPaulies (p:ps) = showPauli p ++ showPaulies ps