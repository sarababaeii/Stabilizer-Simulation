module BitUtils
( bitToBool
, bitToInt
, boolToBit
, boolToInt
, bvToBits
, bitPairToInt
, zeroBV
, singleOneBV
, replaceBit
, splitAtBV
, oneCount
, randomBit
) where

-- Code conventions: bit index i is the i-th qubit from the left, counting from 0. So the rightmost bit is index n-1, where n is the total number of qubits.

import Data.Bit (Bit (unBit))
import Data.Bits (bit, xor, (.&.), zeroBits, complement, shiftR, setBit, popCount) 
import Data.BitVector (BitVector, BV(size), (@@),  toBits, zeroExtend)

import System.Random (StdGen, random, RandomGen (split))

---------------------------------------
-- Type conversions
---------------------------------------
bitToInt :: Bit -> Int
bitToInt 0 = 0
bitToInt 1 = 1

bitToBool :: Bit -> Bool
bitToBool = unBit

boolToBit :: Bool -> Bit
boolToBit False = 0
boolToBit True = 1

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True = 1

bvToBits :: BV -> [Bit]
bvToBits = map boolToBit . toBits

---------------------------------------
-- Bit Operations
---------------------------------------
bitPairToInt :: Bit -> Bit -> Int
bitPairToInt b1 b2 = 2 * bitToInt b1 + bitToInt b2

---------------------------------------
-- BV Constants
---------------------------------------
zeroBV :: Int -> BitVector
zeroBV n = t `xor` t
    where t = singleOneBV n (n - 1)

singleOneBV :: Int -> Int -> BitVector
singleOneBV n i = zeroExtend i (bit (n - i - 1))

---------------------------------------
-- BV Operations
---------------------------------------
replaceBit :: Int -> Bit -> BitVector -> BitVector
replaceBit i 1 bv = setBit bv (size bv - 1 - i)
replaceBit i 0 bv = clearBit' bv (size bv - 1 - i)

clearBit' :: BitVector -> Int -> BitVector  -- O(4n)
clearBit' bv i =
    let nZero = bv .&. zeroBits     -- O(n)
        p = setBit nZero i          -- O(n)
    in bv .&. complement p          -- O(2n)

splitAtBV :: Int -> BitVector -> (BitVector, BitVector)
splitAtBV i bv 
    | i < 0 || i > n = error ("Error: splitAtBV: index out of bounds: " ++ show i ++
                                        " for BitVector of size " ++ show (size bv))
    | otherwise      = (takeRangeBV (0, i) bv n, takeRangeBV (i, size bv) bv n)
    where
        n = size bv

-- [i, j)
takeRangeBV :: (Int, Int) -> BitVector-> Int  -> BitVector
takeRangeBV (i, j) bv n
    | i < 0 || j > n  = error ("Error: takeRangeBV: index out of bounds: " ++ show (i, j) ++
                                            " for BitVector of size " ++ show (size bv))
    | i >= j          = error ("Error: takeRangeBV: invalid range: " ++ show (i, j))                                            
    | otherwise       = bv @@ (n - i - 1, n - j)

oneCount :: BitVector -> Int
oneCount = popCount
---------------------------------------
-- Random bit generation
---------------------------------------
randomBit :: StdGen -> (Bit, StdGen)
randomBit gen = (boolToBit b, gen')
    where (b, gen') = random gen :: (Bool, StdGen)