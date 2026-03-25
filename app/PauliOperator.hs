{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{- HLINT ignore "Use list literal pattern" -}

module PauliOperator
( Pauli(..)
, phaseBool
, bitAt
, identityPauli
, xPauli
, zPauli
, measuringPauli
, applyGate
, groupOp
, isInCompBasis
, firstNonCompBasisPauli
, showPaulies
, pauliToString
, pauliFromString
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

bitAt :: Int -> Pauli -> Bit
bitAt i Pauli {xBits = xs, zBits = zs, phaseBit = r}
  | i < n   = toBit (xs !. i)
  | i < 2 * n = toBit (zs !. (i - n))
  | i == 2 * n = r
  | otherwise = error ("Error: bitAt: index out of bounds: " ++ show i ++ " for Pauli with " ++ show n ++ " qubits")
  where
    n = size xs

identityPauli :: Int -> Pauli
identityPauli n = Pauli {xBits = z, zBits = z, phaseBit = 0}
    where
        z = zeroBV n

xPauli :: Int -> Int -> Pauli
xPauli n i = Pauli {xBits = xs, zBits = zs, phaseBit = 0}
    where
        xs = singleOneBV n i
        zs = zeroBV n

zPauli :: Int -> Int -> Pauli
zPauli n i = Pauli {xBits = xs, zBits = zs, phaseBit = 0}
    where
        xs = zeroBV n
        zs = singleOneBV n i

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
        x = toBit (xs !. i)
        z = toBit (zs !. i)

-- (P, Q -> PQ)
-- different from the paper
groupOp :: Pauli -> Pauli -> Pauli
groupOp p1 p2 =
    let r' = phaseAfterGroupOp p1 p2
        xs' = xBits p1 `xor` xBits p2
        zs' = zBits p1 `xor` zBits p2
    in Pauli {xBits = xs', zBits = zs', phaseBit = r'}

phaseAfterGroupOp :: Pauli -> Pauli -> Bit
phaseAfterGroupOp p1 p2
    | r' `mod` 4 == 0 = 0
    | r' `mod` 4 == 2 = 1
    | otherwise       = error ("Error: Paulies don't commute: " ++ showPauli p1 ++ " and " ++ showPauli p2)
    where
        n = size (xBits p1)
        gs = [g (xBits p1 !. i) (zBits p1 !. i) (toInt (xBits p2 !. i)) (toInt (zBits p2 !. i)) | i <- [0 .. n - 1]]
        r' = 2 * toInt' (phaseBit p1) + 2 * toInt' (phaseBit p2) + sum gs

g :: Bool -> Bool -> Int -> Int -> Int
g False False _ _  = 0
g False True x2 z2 = x2 * (1 - 2 * z2)
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

---------------------------------------
-- Auxiliary functions
---------------------------------------
-- Code conventions: bit index i is the i-th qubit from the left, counting from 0. So the rightmost bit is index n-1, where n is the total number of qubits.

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
    where t = singleOneBV n (n - 1)

singleOneBV :: Int -> Int -> BitVector
singleOneBV n i = zeroExtend i (bit (n - i - 1))

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

pauliToString :: Pauli -> String
pauliToString Pauli {xBits = xs, zBits = zs, phaseBit = r} =
    let n = size xs
        body = [pauliChar (xs !. i) (zs !. i) | i <- [0 .. n - 1]]
        sign = if r == 1 then "-" else ""
    in sign ++ body

pauliChar :: Bool -> Bool -> Char
pauliChar False False = 'I'
pauliChar True False = 'X'
pauliChar False True = 'Z'
pauliChar True True = 'Y'

pauliFromString :: String -> Pauli
pauliFromString str = Pauli {xBits = xs, zBits = zs, phaseBit = r}
    where
        (r, body) = parseSign str
        xs = fromBits [x | c <- body, let (x, _) = pauliBits c]
        zs = fromBits [z | c <- body, let (_, z) = pauliBits c]

pauliBits :: Char -> (Bool, Bool) -- can I convert Bool to Bit?
pauliBits 'I' = (False, False)
pauliBits 'X' = (True , False)
pauliBits 'Z' = (False, True )
pauliBits 'Y' = (True , True )
pauliBits c   = error ("Error: pauliBits: invalid Pauli character: " ++ show c)

parseSign :: String -> (Bit, String)
parseSign ""         = error "Error: parseSign: invalid Pauli string: empty string"
parseSign ('-':[])   = error "Error: parseSign: invalid Pauli string: empty string"
parseSign ('-':xs)   = (1, xs)
parseSign xs         = (0, xs)



-- TODO: implement a more efficient clearBit function, which is O(n) instead of O(4n) ????
-- TODO: lattice operations and Kleene algebra operations.