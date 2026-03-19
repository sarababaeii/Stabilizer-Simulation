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

import Quantum

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
measuringPauli :: Pauli -> Int -> StdGen -> (Pauli, StdGen)
measuringPauli p a gen = (Pauli {xBits = xs, zBits = zs, phaseBit = r}, gen')
    where 
        n = size (xBits p)
        xs = zeroBV n
        zs = zeroExtend a (bit (n - 1 - a))
        (r, gen') = randomBit gen

applyGate :: Gate -> Pauli -> Pauli
applyGate g p =
    let pb = (controlBits g p, targetBits g p)
        r' = phaseAfterGate g pb (phaseBit p)
        (xs', zs') = stateAfterGate g pb p
    in Pauli {xBits = xs', zBits = zs', phaseBit = r'}

-- (P, Q -> PQ)
-- different from the paper
groupOp :: Pauli -> Pauli -> Pauli
groupOp p1 p2 =
    let r' = phaseAfterGroupOp p1 p2
        xs' = (xBits p1) `xor` (xBits p2)
        zs' = (zBits p1) `xor` (zBits p2)
    in Pauli {xBits = xs', zBits = zs', phaseBit = r'}
-- groupOp Pauli {xBits = xs1, zBits = zs1, phaseBit = r1} Pauli {xBits = xs2, zBits = zs2, phaseBit = r2} 
--     |r' `mod` 4 == 0 = Pauli {xBits = xs', zBits = zs', phaseBit = 0}
--     |r' `mod` 4 == 2 = Pauli {xBits = xs', zBits = zs', phaseBit = 1}
--     where
--         xs' = xs1 `xor` xs2
--         zs' = zs1 `xor` zs2
--         gs = [g (xs1 @. i) (zs1 @. i) (toInt (xs2 @. i)) (toInt (zs2 @. i)) | i <- [0 .. (size xs1) - 1]]
--         gSum = sum gs
--         r' = 2 * (toInt' r1) + 2 * (toInt' r2) + gSum


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

-----------------------------
-- Accessing to operated bits
-----------------------------
controlBits :: Gate -> Pauli -> (Bit, Bit)
controlBits CX {control = a, target = _} p = selectedBits a p
controlBits _ _ = (0, 0) -- Default value for H and S

targetBits :: Gate -> Pauli -> (Bit, Bit)
targetBits g p = selectedBits a p
    where a = target g

selectedBits :: Int -> Pauli -> (Bit, Bit)
selectedBits i Pauli {xBits = xs, zBits = zs, phaseBit = _} = (x, z)
    where 
        i' = size xs - 1 - i
        x = toBit (xs @. i')
        z = toBit (zs @. i')

---------------------------------------
-- Updated phase
---------------------------------------
phaseAfterGate :: Gate -> ((Bit, Bit), (Bit, Bit)) -> Bit -> Bit
phaseAfterGate CX {control = _, target = _} ((xa, za), (xb, zb)) r = 
    r `xor` ((xa .&. zb) .&. ((xb `xor` za) `xor` 1))
phaseAfterGate _ (_, (xa, za)) r = r `xor` (xa .&. za)

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

---------------------------------------
-- Updated state after applying a gate
---------------------------------------
stateAfterGate :: Gate -> ((Bit, Bit), (Bit, Bit)) -> Pauli -> (BitVector, BitVector)
stateAfterGate g pb Pauli {xBits = xs, zBits = zs, phaseBit = _} = 
    let xs' = xsAfterGate g pb xs
        zs' = zsAfterGate g pb zs
    in (xs', zs')  

xsAfterGate :: Gate -> ((Bit, Bit), (Bit, Bit)) -> BitVector -> BitVector
xsAfterGate CX {control = _, target = b} ((xa, _), (xb, _)) xs = replaceBit b xb' xs
    where xb' = xa `xor` xb
xsAfterGate H {target = a} (_, (_, za)) xs = replaceBit a za xs
xsAfterGate S {target = _} _ xs = xs

zsAfterGate :: Gate -> ((Bit, Bit), (Bit, Bit)) -> BitVector -> BitVector
zsAfterGate CX {control = a, target = _} ((_, za), (_, zb)) zs = replaceBit a za' zs
    where za' = za `xor` zb
zsAfterGate H {target = a} (_, (xa, _)) zs = replaceBit a xa zs
zsAfterGate S {target = a} (_, (xa, za)) zs = replaceBit a za' zs
    where za' = xa `xor` za

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
-- clearBit' bv i =
--     let n = size bv - 1
--         z = zeroExtend (n - i) (bit i)     -- O(n)
--     in bv .&. (complement p)    -- O(2n)

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