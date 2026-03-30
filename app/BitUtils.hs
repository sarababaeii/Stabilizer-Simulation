module BitUtils
(boolToBit
, boolToInt
, bitToInt
, bvToBits
, zeroBV
, singleOneBV
, replaceBit
, randomBit
) where

-- Code conventions: bit index i is the i-th qubit from the left, counting from 0. So the rightmost bit is index n-1, where n is the total number of qubits.

import Data.Bit
import Data.BitVector
import System.Random (StdGen, random)

---------------------------------------
-- Type conversions
---------------------------------------
boolToBit :: Bool -> Bit
boolToBit False = 0
boolToBit True = 1

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True = 1

bitToInt :: Bit -> Int
bitToInt 0 = 0
bitToInt 1 = 1

bvToBits :: BV -> [Bit]
bvToBits = map boolToBit . toBits

---------------------------------------
-- Constants
---------------------------------------
zeroBV :: Int -> BitVector
zeroBV n = t `xor` t
    where t = singleOneBV n (n - 1)

singleOneBV :: Int -> Int -> BitVector
singleOneBV n i = zeroExtend i (bit (n - i - 1))

---------------------------------------
-- Operations
---------------------------------------
replaceBit :: Int -> Bit -> BitVector -> BitVector
replaceBit i 1 bv = setBit bv (size bv - 1 - i)
replaceBit i 0 bv = clearBit' bv (size bv - 1 - i)

clearBit' :: BitVector -> Int -> BitVector  -- O(4n)
clearBit' bv i =
    let nZero = bv .&. zeroBits     -- O(n)
        p = setBit nZero i          -- O(n)
    in bv .&. complement p          -- O(2n)

---------------------------------------
-- Random bit generation
---------------------------------------
randomBit :: StdGen -> (Bit, StdGen)
randomBit gen = (boolToBit b, gen')
    where (b, gen') = random gen :: (Bool, StdGen)