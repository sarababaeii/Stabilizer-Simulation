module BitUtils
(boolToBit
, boolToInt
, bitToInt
, randomBit
) where

import Data.Bit
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

---------------------------------------
-- Random bit generation
---------------------------------------
randomBit :: StdGen -> (Bit, StdGen)
randomBit gen = (boolToBit b, gen')
    where (b, gen') = random gen :: (Bool, StdGen)