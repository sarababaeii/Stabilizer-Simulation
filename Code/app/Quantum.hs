{-# LANGUAGE DeriveGeneric #-}

module Quantum
( Circuit(..)
, QuantumInstruction(..)
, Gate(..)
, Measure(..)
, MeasureResult(..)
) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

data Circuit = Circuit { qubitNum' :: Int
                       , instructionNum :: Int
                       , instructions :: [QuantumInstruction]
                       } deriving (Show, Read)

data QuantumInstruction = QuantumOp Gate 
                        | ClassicOp Measure 
                        | Skip deriving (Eq, Show, Read)

data Gate = CX { control :: Int
               , target :: Int
               }
          | H { target :: Int }
          | S { target :: Int } deriving (Eq, Show, Read)

data Measure = M { qubit :: Int } deriving (Eq, Show, Read)

data MeasureResult = MR { qubit' :: Int 
                        , result :: Bool
                        , isRandom :: Bool} deriving (Generic, Show, Read)
instance NFData MeasureResult

-- dag? x, y, z?